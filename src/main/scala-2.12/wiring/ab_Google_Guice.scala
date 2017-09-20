package wiring

import com.google.inject._
import com.google.inject.name.{Named, Names}
import net.codingwell.scalaguice.InjectorExtensions._
import net.codingwell.scalaguice.ScalaModule

/**
 * source (scala-guice project):
 * - https://github.com/codingwell/scala-guice
 *
 * nice article with examples:
 * - http://michaelpnash.github.io/guice-up-your-scala/
 */

object aa_Google_Guice {

  def main(args: Array[String]): Unit = {
    val injector = Guice.createInjector(new GuiceTestModule)

    val application = injector.instance[Top]
    application.start()
  }
}

//Injecting

class Top @Inject() (middle: Middle) { def start(): Unit = middle.goDeeper() }
class Middle @Inject() (bottom: Bottom) { def goDeeper() = bottom.goDeeper() }

trait Bottom {def goDeeper()}
class BottomWithUnnamedDependency @Inject() (deepestDependency: DeepestDependency) extends Bottom {def goDeeper() = deepestDependency.goDeeper()}
class BottomWithNamedDependency @Inject() ( @Named("testTwo") deepestDependency: DeepestDependency) extends Bottom {def goDeeper() = deepestDependency.goDeeper()}

trait DeepestDependency {def goDeeper()}
class RealDeepestDependency extends DeepestDependency {def goDeeper() = print("REAL Deepest Dependency")}
class Test1DeepestDependency extends DeepestDependency {def goDeeper() = print("TEST 1 Deepest Dependency")}
class Test2DeepestDependency extends DeepestDependency {def goDeeper() = print("TEST 2 Deepest Dependency")}


// Binding
// Providing

class GuiceProductionModule extends AbstractModule with ScalaModule {

  // 1. Binding trait to provider function
  //
  //@Provides
  //def prodDeepest: DeepestDependency = new RealDeepestDependency

  def configure(): Unit = {
    bind[Bottom].to[BottomWithUnnamedDependency]

    //  2. Binding trait to concrete class
    //  eg: bind[ClazzOrTrait].to[ClazzImpl]
    //
    //bind[DeepestDependency].to[RealDeepestDependency]

    //  3. Binding trait to instance:
    //  eg: bind[ClazzOrTrait].toInstance(myInstance)
    //
    bind[DeepestDependency].toInstance(new RealDeepestDependency)
  }
}

class GuiceTestModule extends AbstractModule with ScalaModule {

  def configure(): Unit = {

    bind[Bottom].to[BottomWithNamedDependency]

    //  4. Binding trait to concrete class - my Name
    //  eg: bind[ClazzOrTrait].annotatedWith(Names.named("test1")).to[ClazzImpl]
    //
    bind[DeepestDependency].annotatedWith(Names.named("testOne")).to[Test1DeepestDependency]
    bind[DeepestDependency].annotatedWith(Names.named("testTwo")).to[Test2DeepestDependency]
  }
}
