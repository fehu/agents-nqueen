package feh.tec.agent.nqueen

import akka.actor.ActorLogging
import feh.tec.agents.comm.NegotiationState.Negotiating
import feh.tec.agents.comm._
import feh.tec.agents.comm.agent._
import NegotiationState._
import feh.tec.agents.comm.{NegotiationVar => NVar}
import PrioritizedNegotiationsFallback._

class Queen(val id: NegotiatingAgentId, val reportTo: SystemAgentRef, boardSize: Int)
  extends NegotiatingAgent
  with MessageDelaying
  with NegotiationReactionBuilder
  with QueenIssuesHandling
  with QueenNegotiationHandling
  with ActorLogging
{
  override lazy val Reporting = new ReportingNegotiationsConfig()

  protected lazy val initializeNegotiations = QueensNegotiation(boardSize)_ :: Nil

  def handleNotInitialized: PartialFunction[Message, Unit] = {
    case NegMsg(msg) if state != AgentState.Initialized => delayMessage(msg)
  }

  def messageReceived = handleNotInitialized orElse handleIssueMessage orElse handleNegotiation

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

trait QueenNegotiationHandling {
  agent: NegotiatingAgent with NegotiationReactionBuilder with MessageDelaying =>

  def handleNegotiation: PartialFunction[Message, Unit] = handleProposal orElse handleProposalResponse

  def handleProposal: PartialFunction[Message, Unit] = {
    case (msg: Proposal) & InState(FallbackState) => delayMessage(msg)
    case (msg: Proposal) & WithLowerPriority()    => respondToProposal(msg)
    case (msg: Proposal) & WithHigherPriority()   =>
      if(!breaksConstrains(msg)) respondToProposal(msg)
      else {
        val neg = negotiation(msg.negotiation)
        neg.set(NVar.State, Negotiating)
        spamCurrentProposal(neg)
        respondToProposal(msg)
      }
  }

  def handleProposalResponse: PartialFunction[Message, Unit] = {
    case (msg: Acceptance) & InState(Negotiating) & AwaitingResponse() => ??? // todo
    case _ => ??? // todo
  }

  def respondToProposal(prop: Proposal) = ???
  def breaksConstrains(prop: Proposal): Boolean = ???

  def spamCurrentProposal(neg: Negotiation) = ???
}