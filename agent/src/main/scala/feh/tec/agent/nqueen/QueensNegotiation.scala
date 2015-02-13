package feh.tec.agent.nqueen

import feh.tec.agents.comm.{Var, NegotiationId, Negotiation}

class QueensNegotiation(varUpdated: Negotiation.VarUpdated[_] => Unit)
  extends Negotiation(QueensNegotiation.id, varUpdated)
{
  defineVar.priority
  defineVar.forIssue(QueensNegotiation.Issues.x)
  defineVar.forIssue(QueensNegotiation.Issues.y)
}

object QueensNegotiation{
  val id = NegotiationId("Queens' positions")

  object Issues{
    val x = Var[Int]("x") // todo: domain, but not obligatory
    val y = Var[Int]("y")
  }
}