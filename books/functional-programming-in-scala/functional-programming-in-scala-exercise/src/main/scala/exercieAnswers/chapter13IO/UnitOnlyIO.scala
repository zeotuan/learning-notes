package exercieAnswers.chapter13IO

/**
 * First iteration of IO implementation to decouple side effect from functional code
 * Not very interesting as it is unable return any result
 * */
trait UnitOnlyIO { self =>
  def unsafeRun: Unit
  def ++(io: UnitOnlyIO): UnitOnlyIO = new UnitOnlyIO {
    override def unsafeRun: Unit = {
      self.unsafeRun
      io.unsafeRun
    }
  }
}

object UnitOnlyIO {
  def PrintLine(msg: String): UnitOnlyIO = new UnitOnlyIO {
     def unsafeRun: Unit = println(msg)
  }

  def empty: UnitOnlyIO = new UnitOnlyIO {
     def unsafeRun: Unit = ()
  }

  case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def winderMsg(po: Option[Player]): String = po.fold("It's a draw")(p => s"${p.name} is the  winner")

  def contest(p1: Player, p2: Player): UnitOnlyIO = PrintLine(winderMsg(winner(p1, p2)))

}
