package feh.tec.agent.nqueen

import feh.tec.agents.comm._

class QueensController(val id: SystemAgentId,
                       queenNeg: NegotiationId,
                       queenCreator: NegotiatingAgentCreator[Queen],
                       queensCount: Int)
  extends NegotiationController
{
  lazy val initialNegotiatorsCreators: Seq[NegotiatingAgentCreator[_]] = List.fill(queensCount)(queenCreator)

  def nameForAgent(creator: NegotiatingAgentCreator[_], count: Int) = creator.role match{
    case Queen.Role => s"Queen-$count"
  }

  def initializeNegotiator(nref: NegotiatingAgentRef): Unit = {
    val scope = negotiators.filter(_ != nref).toSet
    nref ! SystemMessage.Initialize(SystemMessage.SetScope(queenNeg, scope))
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
}
