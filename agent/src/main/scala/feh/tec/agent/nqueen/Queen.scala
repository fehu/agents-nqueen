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

  def handleNegotiation = handleProposal orElse handleProposalResponse orElse handleFallback

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
    case (msg: Acceptance) & InState(Negotiating) & AwaitingResponse() =>
      val neg = negotiation(msg.negotiation)
      setProposalAcceptance(neg, msg, true)
      //todo: guard config fragment
      validateAcceptance(neg)
    case (msg: Acceptance) => // ignore
    case (msg: Rejection) & InState(Negotiating) & AwaitingResponse() & WithHigherPriority() =>
      val neg = negotiation(msg.negotiation)
      setProposalAcceptance(neg, msg, false)
      setNextProposal(neg)
      spamCurrentProposal(neg)
    case (msg: Rejection) & InState(Negotiating) & AwaitingResponse() & WithLowerPriority() =>
      val neg = negotiation(msg.negotiation)
      setProposalAcceptance(neg, msg, true)
      validateAcceptance(neg)
    case (msg: Rejection) => // ignore
  }

  def handleFallback: PartialFunction[Message, Unit] = {
    case (msg: FallbackRequest) & InState(Negotiating) & WithPriorityDiff(-1) =>
      haveToFallback(negotiation(msg.negotiation))
    case (msg: FallbackRequest) & InState(Waiting) & WithPriorityDiff(-1) =>
      val neg = negotiation(msg.negotiation)
      neg.set(NVar.State, Negotiating)
      setNextProposal(neg)
      spamCurrentProposal(neg)
      haveToFallback(neg)
    case (msg: FallbackRequest) & InState(Negotiating | Waiting) & WithHigherPriority() =>
      resendDelayedMessages()
      negotiation(msg.negotiation).set(NVar.State, FallbackState)
    case (msg: FallbackRequest) & InState(Negotiating | Waiting) =>
      resendDelayedMessages()
    case (msg: FallbackRequest) & InState(FallbackState) =>
      delayMessage(msg)
    case (msg: IWillChange) & InState(FallbackState) & AwaitingResponse() =>
      val neg = negotiation(msg.negotiation)
      resetIterator(neg)
      resendDelayedMessages()
      neg.set(NVar.State, Negotiating)
      setNextProposal(neg)
      spamCurrentProposal(neg)
    case (msg: IWillChange) & InState(FallbackState | Negotiating) =>
      delayMessage(msg)
    case (msg: IWillChange) => //ignore
  }

  def setProposalAcceptance(neg: Negotiation, msg: NegotiationResponse, bool: Boolean) =
    neg.transform(NVar.ProposalAcceptance)(_.getOrElse(Map()) + (msg.sender -> bool))

  def respondToProposal(prop: Proposal) = ???
  def breaksConstrains(prop: Proposal): Boolean = ???

  def setNextProposal(neg: Negotiation) = ???
  def spamCurrentProposal(neg: Negotiation) = ???

  def haveToFallback(neg: Negotiation) = {
    resendDelayedMessages()
    //todo: guard failed configuration, if known
    spamIWillChange(neg)
  }
  def spamIWillChange(neg: Negotiation) = ???

  def resetIterator(neg: Negotiation) = ???

  def validateAcceptance(neg: Negotiation) = ???
}