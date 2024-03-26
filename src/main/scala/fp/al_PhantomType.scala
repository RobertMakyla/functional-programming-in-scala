package fp

object al_PhantomType {

  sealed trait DoorState
  sealed trait Open extends DoorState
  sealed trait Closed extends DoorState

  case class Door[State <: DoorState](){
    def open(implicit evidence: State =:= Closed): Door[Open] = Door[Open]()
    def close(implicit evidence: State =:= Open): Door[Closed] = Door[Closed]()
  }

}
