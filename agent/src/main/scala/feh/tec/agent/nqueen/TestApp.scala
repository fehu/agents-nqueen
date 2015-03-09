package feh.tec.agent.nqueen

import akka.actor.{Props, ActorSystem}
import feh.tec.agents.comm._
import ReportLogFormat.Pretty

object TestApp extends App{
  implicit val asys = ActorSystem.create("test")


  val reporter = ReportDistributedPrinter.creator("Queens reporter", "log").create("reporter")

  val controller = QueensController.creator(reporter, 4).create("controller")

  DeafUserAgent.creator("user", {
    agent =>
      import agent._
      controller ! SystemMessage.Start()
      Thread sleep 200
      controller ! ControllerMessage.Begin()
  }).create("user")
}
