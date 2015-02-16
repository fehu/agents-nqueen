package feh.tec.agent.nqueen

import feh.tec.agents.comm.NegotiationState.Negotiating
import feh.tec.agents.comm._
import feh.tec.agents.comm.agent._
import NegotiationState._
import feh.tec.agents.comm.{NegotiationVar => NVar}
import scala.collection.mutable
import PrioritizedNegotiations._

class Queen(val id: NegotiatingAgentId, val reportTo: SystemAgentRef) extends NegotiatingAgent
  with MessageDelaying
  with NegotiationReactionBuilder
  with RegisteringPriorities
  with QueenIssuesHandling
{
  protected lazy val initializeNegotiations = (new QueensNegotiation(_)) :: Nil

  def handleNotInitialized: PartialFunction[Message, Unit] = {
    case NegMsg(msg) if state != AgentState.Initialized => delayMessage(msg)
  }

  def messageReceived = handleNotInitialized orElse handleIssueMessage

  def start() = {
    eachNegotiation(_.set(NVar.State, Negotiating))
    resendDelayedMessages()
  }

  def stop() = {
    eachNegotiation(_.set(NVar.State, Stopped))
  }
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
          if(issueAggregationRequestedBy.size == scope.size) aggregateNextIssueToNegotiation()
        case IssueNegotiation.Remove => issueAggregationRequestedBy.clear()
      }
    case IssueDemand(_, action, issues, _, _) & WithHigherPriority() =>
      action match {
        case IssueNegotiation.Add     => addNewIssues(issues)
        case IssueNegotiation.Remove  => removeIssues(issues)
      }
  }

  override def topPriorityIsKnown() = resendDelayedMessages()

  def aggregateNextIssueToNegotiation() = ???
  def addNewIssues(issue: Seq[Var[_]]) = ???
  def removeIssues(issue: Seq[Var[_]]) = ???
}

object Queen{
  object Role extends NegotiationRole("Chess Queen")

  def creator(reportTo: SystemAgentRef) = AgentCreator(Role){
    id => new Queen(id, reportTo)
  }
}
