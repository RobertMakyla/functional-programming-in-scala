package wiring

/**
 * Pros:
 * - dependencies go only one level. We should never pass dependencies through many levels.
 *
 *   if we put deepestDependency as Top's parameter, we would need to pass it through all layers
 *   painful - too much boilerplate (unnecessarily repeated) code
 *
 * - it's nice in Scala - thanks to main constructor we don't need to save constructor's params as private fields (unlike in Java)
 *
 * - Simpler than Guice, which:
 *   1. doesn't have code navigation
 *   2. requires much more code (modules, binders, providers)
 *   3. requires learning new library to use it effectively.
 */
object ab_Constructors_By_PiotrTarsa {

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

  // to nam zastępuje konfigurację z kontenera DI
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
