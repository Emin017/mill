package mill.meta

import mill.api.daemon.internal.internal
import mill.constants.CodeGenConstants.*
import mill.constants.OutFiles.OutFiles.*
import mill.api.daemon.internal.MillScalaParser
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import mill.internal.Util.backtickWrap

/**
 * @param seenScripts
 * @param repos
 * @param mvnDeps
 * @param importGraphEdges
 * @param errors
 * @param metaBuild If `true`, a meta-build is enabled
 */
@internal
case class FileImportGraph(seenScripts: Map[os.Path, String], errors: Seq[String])

/**
 * Logic around traversing the `import $file` graph, extracting necessary info
 * and converting it to a convenient data structure for downstream code to use
 */
@internal
object FileImportGraph {

  import mill.api.JsonFormatters.pathReadWrite
  implicit val readWriter: upickle.ReadWriter[FileImportGraph] = upickle.macroRW

  /**
   * We perform a depth-first traversal of the import graph of `.mill` files,
   * starting from `build.mill`, collecting the information necessary to
   * instantiate the [[MillRootModule]]
   */
  def parseBuildFiles(
      topLevelProjectRoot: os.Path,
      projectRoot: os.Path,
      output: os.Path,
      parser: MillScalaParser,
      walked: Seq[os.Path],
      colored: Boolean
  ): FileImportGraph = {
    val seenScripts = mutable.Map.empty[os.Path, String]
    val errors = mutable.Buffer.empty[String]

    def processScript(s: os.Path, useDummy: Boolean = false): Unit =
      try {

        val content = if (useDummy) "" else os.read(s)
        val fileName = s.relativeTo(topLevelProjectRoot).toString
        val buildHeaderError =
          if (useDummy) Right(())
          else
            try Right(mill.constants.Util.readBuildHeader(s.toNIO, s.last))
            catch { case e: RuntimeException => Left(e.getMessage) }

        if (s.last.endsWith(".yaml")) seenScripts(s) = os.read(s)
        else buildHeaderError.flatMap(_ => parser.splitScript(content, fileName, colored)) match {
          case Right((prefix, pkgs, stmts)) =>
            val importSegments = pkgs.mkString(".")

            val expectedImportSegments =
              (Seq(rootModuleAlias) ++ (s / os.up).relativeTo(projectRoot).segments)
                .map(backtickWrap)
                .mkString(".")

            val isRootFile = rootBuildFileNames.contains(s.last) && (s / os.up) == projectRoot
            val isBuildFile = buildFileExtensions.asScala.exists(ext => s.last == s"build.$ext")
            val isPackageFile = nestedBuildFileNames.contains(s.last)

            val exactMatch = expectedImportSegments == importSegments
            val emptyPackage = importSegments == ""
            val relaxedMatch =
              importSegments.startsWith("build") ||
                expectedImportSegments.startsWith(importSegments + ".")

            val packageDeclarationValid = (isRootFile, isBuildFile, isPackageFile) match {
              case (true, _, _) => emptyPackage || exactMatch
              case (_, true, _) => emptyPackage || relaxedMatch || exactMatch
              case (_, _, true) => relaxedMatch || exactMatch
              case _ => exactMatch
            }

            if (!packageDeclarationValid) {
              val expectedImport =
                if (expectedImportSegments.isEmpty) "<none>"
                else s"\"package $expectedImportSegments\""
              errors.append(
                s"Package declaration \"package $importSegments\" in " +
                  s"${s.relativeTo(topLevelProjectRoot)} does not match " +
                  s"folder structure. Expected: $expectedImport"
              )
            }
            seenScripts(s) = prefix + stmts.mkString
          case Left(error) =>
            seenScripts(s) = ""
            errors.append(error)
        }
      } catch {
        case ex: Throwable =>
          seenScripts(s) = ""
          errors.append(ex.getClass.getName + " " + ex.getMessage)
      }

    val (useDummy, foundRootBuildFileName) = findRootBuildFiles(projectRoot)

    val foundRootBuildFile = projectRoot / foundRootBuildFileName
    processScript(foundRootBuildFile, useDummy)

    // Check for conflicting build files in the same directory
    val scriptsByDir = walked.groupBy(_ / os.up)
    scriptsByDir.foreach { case (dir, files) =>
      val hasBuildMill =
        files.exists(f => buildFileExtensions.asScala.exists(ext => f.last == s"build.$ext"))
      val hasPackageMill = files.exists(f => nestedBuildFileNames.contains(f.last))

      if (hasBuildMill && hasPackageMill) {
        val conflictingFiles = files.filter(f =>
          buildFileExtensions.asScala.exists(ext => f.last == s"build.$ext") ||
            nestedBuildFileNames.contains(f.last)
        ).map(_.last).mkString(", ")

        errors.append(
          s"Conflicting build files in ${dir.relativeTo(topLevelProjectRoot)}: $conflictingFiles. " +
            s"A directory can contain either build.mill (for root/standalone projects) or " +
            s"package.mill (for submodules), but not both."
        )
      }
    }

    walked.filter(_ != foundRootBuildFile).foreach(processScript(_))

    new FileImportGraph(seenScripts.toMap, errors.toSeq)
  }

  def findRootBuildFiles(projectRoot: os.Path) = {
    val rootBuildFiles = rootBuildFileNames.asScala
      .filter(rootBuildFileName => os.exists(projectRoot / rootBuildFileName))

    val (dummy, foundRootBuildFileName) = rootBuildFiles.toSeq match {
      case Nil => (true, "build.mill")
      case Seq(single) => (false, single)
      case multiple =>
        System.err.println(
          "Multiple root build files found: " + multiple.mkString(",") +
            ", picking " + multiple.head
        )
        (false, multiple.head)
    }

    (dummy = dummy, foundRootBuildFileName = foundRootBuildFileName)
  }

  def walkBuildFiles(projectRoot: os.Path, output: os.Path): Seq[os.Path] = {
    if (!os.exists(projectRoot)) Nil
    else {
      val nestedBuildFileNames = allNestedBuildFileNames.asScala.toSet

      val buildFiles = os
        .walk(
          projectRoot,
          followLinks = true,
          skip = p =>
            p == output ||
              p == projectRoot / millBuild ||
              (os.isDir(p) && !nestedBuildFileNames.exists(n => os.exists(p / n)))
        )
        .filter(p => nestedBuildFileNames.contains(p.last))

      val adjacentScripts = (projectRoot +: buildFiles.map(_ / os.up))
        .flatMap(os.list(_))
        .filter(p => buildFileExtensions.asScala.exists(ext => p.last.endsWith("." + ext)))

      (buildFiles ++ adjacentScripts).distinct
    }
  }

  def fileImportToSegments(base: os.Path, s: os.Path): Seq[String] = {
    val rel = s.relativeTo(base)
    Seq(globalPackagePrefix) ++ Seq.fill(rel.ups)("^") ++ rel.segments
  }
}
