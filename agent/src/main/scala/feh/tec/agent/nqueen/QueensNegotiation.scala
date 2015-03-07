package feh.tec.agent.nqueen

import feh.tec.agents.comm.{NegotiationVar, Var, NegotiationId, Negotiation}

class QueensNegotiation(varUpdated: Negotiation.VarUpdated[_ <: NegotiationVar] => Unit, boardSize: Int)
  extends Negotiation(QueensNegotiation.id, varUpdated)
{
  defineVar.priority
  defineVar.forIssue(QueensNegotiation.Vars.x, 1 to boardSize)
  defineVar.forIssue(QueensNegotiation.Vars.y, 1 to boardSize)
}

object QueensNegotiation{
  val id = NegotiationId("Queens' positions")

  object Vars{
    val x = Var[Int]("x")
    val y = Var[Int]("y")
  }

  def apply(boardSize: Int)(varUpdated: Negotiation.VarUpdated[_ <: NegotiationVar] => Unit) = new QueensNegotiation(varUpdated, boardSize)
}