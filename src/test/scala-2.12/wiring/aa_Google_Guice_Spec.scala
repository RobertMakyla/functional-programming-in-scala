package wiring

import com.google.inject.{Guice, Injector, Module}
import net.codingwell.scalaguice.InjectorExtensions._
import org.scalatest.{FreeSpec, MustMatchers}

class aa_Google_Guice_Spec extends FreeSpec with MustMatchers {

  "Wiring should work in " - {

    "prod module" in {
      verifyDI(new GuiceProductionModule)
    }

    "test module" in {
      verifyDI(new GuiceTestModule)
    }
  }

  def verifyDI(module: Module) = {
    val injector: Injector = Guice.createInjector(module)
    val component: Top = injector.instance[Top]
    component.start()
  }

}
