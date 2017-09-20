package wiring
import com.softwaremill.macwire._

// Mc Wire (DI for scala, by Warski: http://di-in-scala.github.io/)
/**
 * How it works:
 *
 * - Reflection - ability of code to inspect and modify itself
 * - Scala Macros - it's scala ability of reflection ( shipped in scala 2.10 - still not official in 2.12)
 *
 * - the 'wire[ ]' macro - generates new instances
 *
 */

/**
 * Pros
 * - type-checking at compile time
 */

/**
 * How 'wire[ ]' works:
 *
 * - wire tries to find:
 *  1. constructor annotated with @Inject
 *  2. then, non-private constructor
 *  3. then, apply() in companion object
 *
 * - for each search it's looking in:
 *  1. current block
 *  2. then, in enclosing type
 *  3. then, in parent types.
 *
 *  - a compile-time error occurs if:
 *  1. if there are no/multiple/implicits values of given type in enclosing block, enclosing type or parent types.
 */
object ae_McWire_By_Warski {

  def main(args: Array[String]): Unit = {
    val application = ProductionModule.top
    application.start()
  }

  class Top(middle: Middle) { def start(): Unit = middle.goDeeper() }

  class Middle(bottom: Bottom){def goDeeper()=bottom.goDeeper()}

  class Bottom(deepestDependency: DeepestDependency){def goDeeper() = deepestDependency.goDeeper()}

  trait DeepestDependency  {def goDeeper()}
  class RealDeepestDependency extends DeepestDependency {def goDeeper() = println("Real one")}
  class TestDeepestDependency extends DeepestDependency {def goDeeper() = println("Test one")}

  object ProductionModule {

    // Multiple Implementations
    val isProd = true
    lazy val deepestDependency =
      if (isProd) wire[RealDeepestDependency] else wire[TestDeepestDependency]

    // Example of compile-time error :
    //lazy val anotherValueCausingWireError = new TestDeepestDependency

    lazy val bottom = wire[Bottom]
    lazy val middle = wire[Middle]

    // lazy vals / vals - are singletons
    //lazy val top = wire[Top]

    // different instance each time:
    def top = wire[Top]
  }

}
