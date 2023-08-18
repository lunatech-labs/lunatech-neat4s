package com.lunatech.neat4s

import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.stream.{Attributes, Inlet, Outlet, Shape}
import MapListElementsToOutputs.ListFanOutShape

import scala.collection.immutable

object MapListElementsToOutputs {

  final case class ListFanOutShape[T](in: Inlet[Iterable[T]], out: Seq[Outlet[T]]) extends Shape {
    def inlets: immutable.Seq[Inlet[_]] = in :: Nil
    def outlets: immutable.Seq[Outlet[_]] = out
    def deepCopy(): Shape = new ListFanOutShape[T](in, out)
  }

  def apply[T](outputPorts: Int): GraphStage[ListFanOutShape[T]] =
    new MapListElementsToOutputs[T](outputPorts)
}

final class MapListElementsToOutputs[T](val outputPorts: Int) extends GraphStage[ListFanOutShape[T]] {
  import MapListElementsToOutputs._

  val in = Inlet[Iterable[T]]("ListElementsToOutputs.in")
  val out: immutable.IndexedSeq[Outlet[T]] =
    Vector.tabulate(outputPorts)(i => Outlet[T]("ListElementsToOutputs.out" + i))

  override val shape = ListFanOutShape(in, out)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler {

      private var pullsReceived = 0
      private def pullsPending: Boolean = pullsReceived != out.size

      override def onPush(): Unit = {
        if (isAvailable(in)) {
          val list = grab(in)
          if (list.size != out.size) {
            cancelStage(new IllegalArgumentException(
              s"received a list with ${list.size} elements but this operator has been created with ${out.size} outlets - either could be wrong, I don't know which :shrug"))
          } else {
            out.zip(list).foreach {
              case (outlet, elem) => push(outlet, elem)
            }
          }
        }
      }
      setHandler(in, this)

      out.foreach { o =>
        setHandler(
          o,
          new OutHandler {
            override def onPull(): Unit = {
              pullsReceived += 1
              if (!pullsPending) {
                pullsReceived = 0
                pull(in)
              }
            }
          })
      }
    }
}
