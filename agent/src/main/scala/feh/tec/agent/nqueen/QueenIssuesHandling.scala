package feh.tec.agent.nqueen

import feh.tec.agents.comm._
import feh.tec.agents.comm.agent._
import PrioritizedNegotiationsFallback._
import scala.collection.mutable
import feh.tec.agents.comm.{NegotiationVar => NVar}

trait QueenIssuesHandling
  extends RegisteringPriorities
  with DomainIterators
{
  agent: NegotiatingAgent with MessageDelaying with NegotiationReactionBuilder =>

  lazy val issueAggregationRequestedBy = mutable.HashSet.empty[NegotiatingAgentRef]

  def handleIssueMessage: PartialFunction[Message, Unit] = {
    case req: IssueRequest if isTopPriority(req.negotiation).isEmpty => delayMessage(req)
    case req: IssueRequest if !isTopPriority(req.negotiation).get => //ignore
    case req: IssueRequest =>
      req.action match {
        case IssueNegotiation.Add =>
          issueAggregationRequestedBy += req.sender
          val scope = negotiation(req.negotiation) apply NegotiationVar.Scope
          if(issueAggregationRequestedBy.size == scope.size) aggregateNextIssueToNegotiation(req.negotiation)
        case IssueNegotiation.Remove => issueAggregationRequestedBy.clear()
      }
    case IssueDemand(neg, action, issues, _, _) & WithHigherPriority() =>
      action match {
        case IssueNegotiation.Add     => addNewIssues(neg, issues)
        case IssueNegotiation.Remove  => removeIssues(neg, issues)
      }
    case (_: IssueDemand) & WithLowerPriority() => //ignore
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
    resetDomainIterator(negId)
  }

  def resetDomainIterator(negId: NegotiationId) = {
    val neg = negotiation(negId)
    val dit = iteratorFor(negId, neg(NVar.CurrentIssues))
    neg.set(NVar.DomainIterator, dit)
  }

  def negotiationFinished(negId: NegotiationId)
}
