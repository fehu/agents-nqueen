package feh.tec.agent.nqueen

import feh.tec.agents.comm._

class QueensController(val id: SystemAgentId,
                       queenNeg: NegotiationId,
                       queenCreator: NegotiatingAgentCreator[Queen],
                       queensCount: Int)
  extends NegotiationController with NegotiationController.InitialAgents
{
  lazy val initialNegotiatorsCreators: Seq[NegotiatingAgentCreator[_]] = List.fill(queensCount)(queenCreator)

  def nameForAgent(role: NegotiationRole, index: Int) = role match{
    case Queen.Role => s"Queen-$index"
  }

  private var priorityToAssign = 0
  protected def nextPriority = {
    priorityToAssign += 1
    priorityToAssign
  }

  def initializeNegotiator(nref: NegotiatingAgentRef): Unit = {
    val scope = negotiators.filter(_ != nref).toSet
    nref ! SystemMessage.Initialize(
      SystemMessage.SetScope(queenNeg, scope),
      SystemMessage.SetPriority(queenNeg, nextPriority)
    )
  }

  def systemMessageReceived: PartialFunction[SystemMessage, Unit] = {
    case _: SystemMessage.Start =>
      start()
      initialize()
    case _: SystemMessage.Stop => stop()
    case _: ControllerMessage.Begin => startNegotiation()
  }

  def messageReceived: PartialFunction[Message, Unit] = Map()

  protected def onMessageSent(msg: Message, to: AgentRef): Unit = {}

  protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit = {}

  protected def unknownSystemMessage(sysMsg: SystemMessage): Unit = sys.error("unknownSystemMessage: " + sysMsg)

  override def supervisorStrategy = NegotiationController.Supervision.StopAll
}

object QueensController{
  val Role = SystemAgentRole("Queens Controller")

  def creator(reportTo: SystemAgentRef, boardSize: Int) = AgentCreator(Role){
    id => new QueensController(id, QueensNegotiation.id, Queen.creator(reportTo, boardSize), boardSize)
  }
}