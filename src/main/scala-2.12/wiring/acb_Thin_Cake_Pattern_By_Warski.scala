package wiring
import com.softwaremill.macwire._

import wiring.tickets.TicketsModule
import wiring.train.TrainModule
import wiring.wars.WarsModule

//http://www.warski.org/blog/2014/02/using-scala-traits-as-modules-or-the-thin-cake-pattern/

/**
 * Dividing wiring into small pieces - and enclosing it in traits (modules).
 *
 * We can create PRE-WIRED trait module per package.
 * Since it's one package classes probably cooperate together and can be wired in one module.
 *
 * Like this we ship package not only with code but with wired object graph.
 *
 * The wiring itself can be done Manually or using Mc Wire...
 */

object TrainStation extends App {
  val modules = new TicketsModule with WarsModule with TrainModule
  modules.train.go()
}

package tickets {
  class TicketTerminal
  class TicketController(terminal: TicketTerminal)

  // each package has trait module
  trait TicketsModule {
    lazy val terminal = wire[TicketTerminal]
    lazy val ticketController = wire[TicketController]
  }
}

package wars {
  class WarsTerminal
  class WarsMenu
  class WarsCounter(repo: WarsMenu, terminal: WarsTerminal)

  trait WarsModule {
    lazy val warsTerminal = wire[WarsTerminal]
    lazy val menu = wire[WarsMenu]
    lazy val warsCounter = wire[WarsCounter]
  }
}

package train {
  import tickets._
  import wars._

  class TrainTimetable

  class Train(
    timetable: TrainTimetable,
    ticketController: TicketController,
    restaurant: WarsCounter) {
    def go() {println("chooo chooo :) ")}
  }

  trait TrainModule {
    lazy val timetable = wire[TrainTimetable]
    lazy val train = wire[Train]

    // Interceptors - implementation depends on other modules
    def warsCounter: WarsCounter
    def ticketController: TicketController
  }
}