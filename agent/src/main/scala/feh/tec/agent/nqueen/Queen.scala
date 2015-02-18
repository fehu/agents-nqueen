package feh.tec.agent.nqueen

import feh.tec.agents.comm.NegotiationState.Negotiating
import feh.tec.agents.comm._
import feh.tec.agents.comm.agent._
import NegotiationState._
import feh.tec.agents.comm.{NegotiationVar => NVar}
import scala.collection.mutable
import PrioritizedNegotiations._

class Queen(val id: NegotiatingAgentId, val reportTo: SystemAgentRef, boardSize: Int) extends NegotiatingAgent
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

trait QueenIssuesHandling extends RegisteringPriorities{
  agent: NegotiatingAgent with MessageDelaying with NegotiationReactionBuilder =>

  lazy val issueAggregationRequestedBy = mutable.HashSet.empty[NegotiatingAgentRef]

  def handleIssueMessage: PartialFunction[Message, Unit] = {
    case req: IssueRequest if isTopPriority(req.negotiation).isEmpty => delayMessage(req)
    case req: IssueRequest if !isTopPriority(req.negotiation).get => //ignore
    case req: IssueRequest =>
      req.action match {
        case IssueNegotiation.Add =>
          issueAggregationRequestedBy += req.sender.asInstanceOf[NegotiatingAgentRef]
          val scope = negotiation(req.negotiation) apply NegotiationVar.Scope
          if(issueAggregationRequestedBy.size == scope.size) aggregateNextIssueToNegotiation(req.negotiation)
        case IssueNegotiation.Remove => issueAggregationRequestedBy.clear()
      }
    case IssueDemand(neg, action, issues, _, _) & WithHigherPriority() =>
      action match {
        case IssueNegotiation.Add     => addNewIssues(neg, issues)
        case IssueNegotiation.Remove  => removeIssues(neg, issues)
      }
  }

  override def topPriorityIsKnown() = resendDelayedMessages()

  def aggregateNextIssueToNegotiation(negId: NegotiationId) = {
    val neg = negotiation(negId)
    val issuesDiff = neg.issues diff neg(NegotiationVar.CurrentIssues)
    if(issuesDiff.isEmpty) negotiationFinished(negId)
    else {
      val nextIssue = issuesDiff.head
      addNewIssues(negId, nextIssue :: Nil)
      neg(NVar.Scope).foreach(_ ! IssueDemand(negId, IssueNegotiation.Add, nextIssue :: Nil, neg(NVar.Priority)))
    }
  }
  def addNewIssues(negId: NegotiationId, issues: Seq[Var[_]]) = {
    val issue = issues.ensuring(_.size == 1).head
    negotiation(negId).transform(NVar.CurrentIssues){
      case None => issue :: Nil
      case Some(currentIssues) => 
        assert(!currentIssues.contains(issue))
        currentIssues :+ issue
    }
    resetDomainIterator(negId)
  }
  def removeIssues(negId: NegotiationId, issues: Seq[Var[_]]) = {
    val issue = issues.ensuring(_.size == 1).head
    negotiation(negId).transform(NVar.CurrentIssues){
      case None => sys.error("No issue to remove")
      case Some(currentIssues) => currentIssues
        .ensuring(_.contains(issue), s"$issue is not among current issues")
        .filter(_ != issue)
    }
  }

  def resetDomainIterator(negId: NegotiationId) = ???
  
  def negotiationFinished(negId: NegotiationId) = ???
}

object Queen{
  object Role extends NegotiationRole("Chess Queen")

  def creator(reportTo: SystemAgentRef, boardSize: Int) = AgentCreator(Role){
    id => new Queen(id, reportTo, boardSize)
  }
}
