package mill.integration

import mill.testkit.UtestIntegrationTestSuite

import utest.*

object RootModuleCompileErrorTests extends UtestIntegrationTestSuite {
  val tests: Tests = Tests {
    test - integrationTest { tester =>
      import tester.*
      val res = eval("foo.scalaVersion")

      assert(!res.isSuccess)

      locally {
        // For now these error messages still show generated/mangled code; not ideal, but it'll do
        assert(res.err.contains("""build.mill:4:67"""))
        assert(res.err.contains("""Not found: type UnknownRootModule"""))
        assert(res.err.contains(
          """abstract class package_  extends _root_.mill.util.MainRootModule, UnknownRootModule {"""
        ))
        assert(
          res.err.contains(
            """                                                                  ^^^^^^^^^^^^^^^^^"""
          )
        )
      }

      locally {
        // For now these error messages still show generated/mangled code; not ideal, but it'll do
        assert(res.err.replace('\\', '/').contains("""foo/package.mill:3:113"""))
        assert(res.err.contains("""Not found: type UnknownFooModule"""))
        assert(res.err.contains(
          """abstract class package_  extends _root_.mill.api.internal.SubfolderModule(_root_.build_.package_.millDiscover), UnknownFooModule {"""
        ))
        assert(res.err.contains(
          """                                                                                                                ^^^^^^^^^^^^^^^^"""
        ))
      }

      locally {
        assert(res.err.contains("""build.mill:5:22"""))
        assert(res.err.contains("""Not found: unknownRootInternalDef"""))
        assert(res.err.contains("""def scalaVersion = unknownRootInternalDef"""))
        assert(res.err.contains("""                     ^^^^^^^^^^^^^^^^^^^^^^"""))
      }

      locally {
        assert(res.err.contains("""build.mill:17:25"""))
        assert(res.err.contains("""Not found: type UnknownBeforeModule"""))
        assert(res.err.contains("""object before extends UnknownBeforeModule"""))
        assert(res.err.contains("""                        ^^^^^^^^^^^^^^^^^^^"""))
      }

      locally {
        assert(res.err.contains("""build.mill:9:22"""))
        assert(res.err.contains("""Not found: type UnknownAfterModule"""))
        assert(res.err.contains("""object after extends UnknownAfterModule"""))
        assert(res.err.contains("""                     ^^^^^^^^^^^^^^^^^^"""))
      }

      locally {
        assert(res.err.replace('\\', '/').contains("""foo/package.mill:4:22"""))
        assert(res.err.contains("""Not found: unknownFooInternalDef"""))
        assert(res.err.contains("""def scalaVersion = unknownFooInternalDef"""))
        assert(res.err.contains("""                     ^^^^^^^^^^^^^^^^^^^^^"""))
      }

      locally {
        assert(res.err.replace('\\', '/').contains("""foo/package.mill:16:25"""))
        assert(res.err.contains("""Not found: type UnknownBeforeFooModule"""))
        assert(res.err.contains("""object before extends UnknownBeforeFooModule"""))
        assert(res.err.contains("""                      ^^^^^^^^^^^^^^^^^^^^^^"""))
      }

      locally {
        assert(res.err.replace('\\', '/').contains("""foo/package.mill:8:22"""))
        assert(res.err.contains("""Not found: type UnknownAfterFooModule"""))
        assert(res.err.contains("""object after extends UnknownAfterFooModule"""))
        assert(res.err.contains("""                     ^^^^^^^^^^^^^^^^^^^^^"""))
      }
    }
  }
}
