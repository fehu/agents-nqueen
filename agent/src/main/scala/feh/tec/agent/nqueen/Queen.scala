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
  override lazy val Reporting = new ReportingNegotiationsConfig(messageReceived = true)

  protected lazy val initializeNegotiations = QueensNegotiation(boardSize)_ :: Nil

  def handleNotInitialized: PartialFunction[Message, Unit] = {
    case NegMsg(msg) if state != AgentState.Initialized => delayMessage(msg)
  }

  def messageReceived = handleNotInitialized orElse handleIssueMessage orElse handleNegotiation

  def start() = {
    eachNegotiation(
      _.addNegVarDefaults(
        NVar.CurrentIssues -> Some(Nil),
        NVar.AwaitingResponse -> Some(None),
        NVar.CurrentProposal -> None,
        NVar.DomainIterator -> None
//        NVar.ProposalAcceptance -> Some(Map())
      ),
      _.set(NVar.State)(Negotiating),
      _.set(NVar.CurrentIssues)(Nil),
      neg => spamMessage(neg, IssueRequest(neg.id, IssueNegotiation.Add, Nil, neg(NVar.Priority)))
    )
    resendDelayedMessages()
  }

  def stop() = {
    eachNegotiation(_.set(NVar.State)(Stopped))
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
  agent: NegotiatingAgent with NegotiationReactionBuilder with MessageDelaying with QueenIssuesHandling =>

  def handleNegotiation = handleProposal orElse handleProposalResponse orElse handleFallback

  def issuesAdded(negId: NegotiationId, issues: Seq[Var[_]]) = {
    val neg = negotiation(negId)
    if(neg.get(NVar.CurrentProposal).isEmpty) setNextProposal(neg)
    spamCurrentProposal(neg)
  }
  def issuesRemoved(negId: NegotiationId, issues: Seq[Var[_]]) = {}

  def handleProposal: PartialFunction[Message, Unit] = {
    case (msg: Proposal) & InState(FallbackState) => delayMessage(msg)
    case (msg: Proposal) & WithLowerPriority()    => respondToProposal(msg, !breaksConstrains(msg))
    case (msg: Proposal) & WithHigherPriority()   =>
      if(!breaksConstrains(msg)) respondToProposal(msg, acceptance = true)
      else {
        val neg = negotiation(msg.negotiation)
        neg.set(NVar.State)(Negotiating)
        spamCurrentProposal(neg)
        respondToProposal(msg, acceptance = false)
      }
  }

  def handleProposalResponse: PartialFunction[Message, Unit] = {
    case (msg: Acceptance) & InState(Negotiating) & AwaitingResponse() =>
      val neg = negotiation(msg.negotiation)
      setProposalAcceptance(neg, msg, acceptance = true)
      //todo: guard config fragment
      validateAcceptance(neg)
    case (msg: Acceptance) => // ignore
    case (msg: Rejection) & InState(Negotiating) & AwaitingResponse() & WithHigherPriority() =>
      val neg = negotiation(msg.negotiation)
      setProposalAcceptance(neg, msg, acceptance = false)
      setNextProposal(neg)
      spamCurrentProposal(neg)
    case (msg: Rejection) & InState(Negotiating) & AwaitingResponse() & WithLowerPriority() =>
      val neg = negotiation(msg.negotiation)
      setProposalAcceptance(neg, msg, acceptance = true)
      validateAcceptance(neg)
    case (msg: Rejection) => // ignore
  }

  def handleFallback: PartialFunction[Message, Unit] = {
    case (msg: FallbackRequest) & InState(Negotiating) & WithPriorityDiff(-1) =>
      haveToFallback(negotiation(msg.negotiation), msg)
    case (msg: FallbackRequest) & InState(Waiting) & WithPriorityDiff(-1) =>
      val neg = negotiation(msg.negotiation)
      neg.set(NVar.State)(Negotiating)
      setNextProposal(neg)
      spamCurrentProposal(neg)
      haveToFallback(neg, msg)
    case (msg: FallbackRequest) & InState(Negotiating | Waiting) & WithHigherPriority() =>
      resendDelayedMessages()
      negotiation(msg.negotiation).set(NVar.State)(FallbackState)
    case (msg: FallbackRequest) & InState(Negotiating | Waiting) =>
      resendDelayedMessages()
    case (msg: FallbackRequest) & InState(FallbackState) =>
      delayMessage(msg)
    case (msg: IWillChange) & InState(FallbackState) & AwaitingResponse() =>
      val neg = negotiation(msg.negotiation)
      resetDomainIterator(neg.id)
      resendDelayedMessages()
      neg.set(NVar.State)(Negotiating)
      setNextProposal(neg)
      spamCurrentProposal(neg)
    case (msg: IWillChange) & InState(FallbackState | Negotiating) =>
      delayMessage(msg)
    case (msg: IWillChange) => //ignore
  }

  def breaksConstrains(prop: Proposal): Boolean = ???

  def setProposalAcceptance(neg: Negotiation, msg: NegotiationResponse, acceptance: Boolean) =
    neg.transform(NVar.ProposalAcceptance)(_ + (msg.sender -> acceptance))

  def respondToProposal(prop: Proposal, acceptance: Boolean) = { // todo: isn't exactly as described
    val neg = negotiation(prop.negotiation)
    val myVals = neg.issueValues
    val myPriority = neg(NVar.Priority)
    val response =
      if(acceptance) Acceptance(prop.negotiation, prop.uuid, myVals, myPriority)
      else Rejection(prop.negotiation, prop.uuid, myVals, myPriority)
    prop.sender ! response
  }

  def setNextProposal(neg: Negotiation) = {
    val it = neg(NVar.DomainIterator)
    if(it.hasNext) neg.set(NVar.CurrentProposal)(Proposal(neg.id, it.next(), neg(NVar.Priority)))//(NVar.Default.Stubs.CurrentProposal)// todo !!!!!!!!!!!!!!!!!!!!!
    else {
      spamMessage(neg, IssueRequest(neg.id, IssueNegotiation.Remove, Nil, neg(NVar.Priority)))
      val fallback = FallbackRequest(neg.id, neg(NVar.Priority))
      spamMessage(neg, fallback)
      neg.set(NVar.State)(FallbackState)
      neg.set(NVar.AwaitingResponse)(Some(fallback.uuid))
    }
  }

  def spamMessage(neg: Negotiation, msg: NegotiationMessage) = neg(NVar.Scope).foreach(_ ! msg)

  def spamCurrentProposal(neg: Negotiation) = spamMessage(neg, neg(NVar.CurrentProposal)) // (NVar.Default.Stubs.CurrentProposal)

  def haveToFallback(neg: Negotiation, request: FallbackRequest) = {
    resendDelayedMessages()
    //todo: guard failed configuration, if known
    spamIWillChange(neg, request)
  }
  def spamIWillChange(neg: Negotiation, request: FallbackRequest) =
    spamMessage(neg, IWillChange(neg.id, request.uuid, neg(NVar.Priority)))

  def validateAcceptance(neg: Negotiation) = { //todo: configurations
    neg.set(NVar.State)(Waiting)
    spamMessage(neg, IssueRequest(neg.id, IssueNegotiation.Add, Nil, neg(NVar.Priority)))
    spamCurrentProposal(neg)
  }
}