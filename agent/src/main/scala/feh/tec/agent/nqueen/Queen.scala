package feh.tec.agent.nqueen

import feh.tec.agents.comm.NegotiationState.Negotiating
import feh.tec.agents.comm._
import feh.tec.agents.comm.agent._
import NegotiationState._
import feh.tec.agents.comm.{NegotiationVar => NVar}

class Queen(val id: NegotiatingAgentId, val reportTo: SystemAgentRef, boardSize: Int)
  extends NegotiatingAgent
  with MessageDelaying
  with NegotiationReactionBuilder
  with RegisteringPriorities
  with QueenIssuesHandling
{
  protected lazy val initializeNegotiations = QueensNegotiation(boardSize)_ :: Nil

  def handleNotInitialized: PartialFunction[Message, Unit] = {
    case NegMsg(msg) if state != AgentState.Initialized => delayMessage(msg)
  }

  def messageReceived = handleNotInitialized orElse handleIssueMessage

  def start() = {
    eachNegotiation(
      _.set(NVar.State, Negotiating),
      _.set(NVar.CurrentIssues, Nil)
    )
    resendDelayedMessages()
  }

  def stop() = {
    eachNegotiation(_.set(NVar.State, Stopped))
  }

  def negotiationFinished(negId: NegotiationId): Unit = ???
}

object Queen{
  object Role extends NegotiationRole("Chess Queen")

  def creator(reportTo: SystemAgentRef, boardSize: Int) = AgentCreator(Role){
    id => new Queen(id, reportTo, boardSize)
  }
}
