package feh.tec.agent.nqueen

import akka.actor.ActorSystem
import feh.tec.agents.comm._
import feh.tec.agents.comm.agent.{MessageDelaying, NegotiationReactionBuilder}
import NegotiationState._
import feh.tec.agents.comm.{NegotiationVar => NVar}

class Queen(val id: NegotiatingAgentId , val reportTo: SystemAgentRef) extends NegotiatingAgent
  with MessageDelaying
  with NegotiationReactionBuilder
{
  protected lazy val initializeNegotiations = (new QueensNegotiation(_)) :: Nil

  def messageReceived = {
    case NegMsg(msg & InState(NotInitialized)) => delayMessage(msg)
  }

  def start() = {
    eachNegotiation(_.set(NVar.State, NotInitialized))
  }

  def stop() = {
    eachNegotiation(_.set(NVar.State, Stopped))
  }
}

object Queen{
  object Role extends NegotiationRole("Chess Queen")

  def creator(reportTo: SystemAgentRef) = AgentCreator(Role){
    id => new Queen(id, reportTo)
  }
}


object Test extends App{
  implicit val asys = ActorSystem.create("test")

  val qc = Queen.creator(null)

  val q1 = qc.create("q-1")

  println(q1)
}