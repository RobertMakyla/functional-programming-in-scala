package wiring

/**
 * Pros:
 *
 * - dependencies are checked an compile-time
 *
 * - Safes start-up time: no run-time reflection, scanning for annotations, DI containers to start
 * - Flexibility: we are free from any constraint of framework - more flexibility
 * - Simpler than Guice, because:
 *
 * 1. Guice doesn't have code navigation
 * 2. Guice requires more code (modules, binders, providers)
 * 3. Guice requires learning (as new framework) to use it effectively.
 *
 */

/**
 * Cons:
 * - manually creating and wiring each instance may be tedious.
 */

/**
 * To remember:
 *
 * - for small projects (micro services) it should be enough
 *
 * - dependencies should go only one level. We should never pass dependencies through many levels.
 *
 * if we put deepestDependency as Top's parameter, we would need to pass it through all layers
 * painful - too much boilerplate (unnecessarily repeated) code
 */
object aa_Manual_DI {

  def main(args: Array[String]): Unit = {
    val application = ProductionModule.top
    application.start()
  }

  class Top(middle: Middle) { def start(): Unit = println("the app has started") }

  class Middle(bottom: Bottom)

  class Bottom(deepestDependency: DeepestDependency)

  trait DeepestDependency
  class RealDeepestDependency extends DeepestDependency
  class TestDeepestDependency extends DeepestDependency

  //this it the part that normally the DI CONTAINER does:
  object ProductionModule {
    val deepestDependency = new RealDeepestDependency
    val bottom = new Bottom(deepestDependency)
    val middle = new Middle(bottom)
    val top = new Top(middle)
  }

  object TestModule {
    val deepestDependency = new TestDeepestDependency
    val bottom = new Bottom(deepestDependency)
    val middle = new Middle(bottom)
    val top = new Top(middle)
  }

}
