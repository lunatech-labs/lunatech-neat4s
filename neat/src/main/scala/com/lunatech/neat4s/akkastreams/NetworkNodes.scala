package com.lunatech.neat4s.akkastreams

import akka.NotUsed
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL}
import akka.stream.stage.{GraphStage, GraphStageLogic, GraphStageWithMaterializedValue, InHandler, OutHandler}
import akka.stream.{Attributes, Graph, Inlet, Outlet, Shape, UniformFanInShape, UniformFanOutShape}
import com.lunatech.neat4s.NEAT.ActivationFunction
import NetworkNode.OutputValueCollector.OutputValueCollectorShape

import scala.concurrent.{Future, Promise}

object NetworkNode {

  type InputNodeShape = UniformFanOutShape[BigDecimal, BigDecimal]
  type OutputNodeShape = UniformFanInShape[BigDecimal, BigDecimal]

  def createInputNode(outputPorts: Int, activationFunction: ActivationFunction): Graph[InputNodeShape, NotUsed] =
    GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val input = b.add(createInput(weight = 1))
      val activation = b.add(createActivation(activationFunction))
      val broadcast = b.add(Broadcast[BigDecimal](outputPorts))

      input.out ~> activation.in
      activation.out ~> broadcast.in

      UniformFanOutShape(input.in, broadcast.outlets: _*)
    }

  def createOutputNode(inputPorts: Int, activationFunction: ActivationFunction): Graph[OutputNodeShape, NotUsed] =
    GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val inputs = Vector.tabulate(inputPorts)(_ => b.add(createInput(1)))
      val sumInputs = b.add(SumNodeInput(inputPorts))
      val activation = b.add(createActivation(activationFunction))

      inputs.zipWithIndex.foreach {
        case (in, i) => in.out ~> sumInputs.inlets(i)
      }
      sumInputs ~> activation

      UniformFanInShape(activation.out, inputs.map(_.in): _*)
    }

  def createHiddenNode(
      inputPorts: Int,
      outputPorts: Int,
      activationFunction: ActivationFunction): Graph[HiddenNodeShape, NotUsed] = {
    GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val inputs = Vector.tabulate(inputPorts)(_ => b.add(createInput(1)))
      val sumInputs = b.add(SumNodeInput(inputPorts))
      val activation = b.add(createActivation(activationFunction))
      val broadcast = b.add(Broadcast[BigDecimal](outputPorts))

      inputs.zipWithIndex.foreach {
        case (in, i) => in.out ~> sumInputs.inlets(i)
      }

      sumInputs ~> activation
      activation.out ~> broadcast.in

      HiddenNodeShape(inputs.map(_.in), broadcast.outlets)
    }
  }

  def createInput(weight: BigDecimal): Flow[BigDecimal, BigDecimal, NotUsed] =
    Flow[BigDecimal].map(_ * weight)

  def createActivation(activationFunction: ActivationFunction): Flow[BigDecimal, BigDecimal, NotUsed] =
    Flow[BigDecimal].map(activationFunction)

  final case class HiddenNodeShape(in: Seq[Inlet[BigDecimal]], out: Seq[Outlet[BigDecimal]]) extends Shape {
    override def inlets: Seq[Inlet[_]] = in
    override def outlets: Seq[Outlet[_]] = out
    override def deepCopy(): Shape = HiddenNodeShape(in.map(_.carbonCopy()), out.map(_.carbonCopy()))
  }

  object SumNodeInput {
    def apply(inputPorts: Int): SumNodeInput =
      new SumNodeInput(inputPorts)
  }

  final class SumNodeInput(inputPorts: Int) extends GraphStage[UniformFanInShape[BigDecimal, BigDecimal]] {

    val in = Vector.tabulate(inputPorts)(i => Inlet[BigDecimal]("SumNodeInput.in" + i))
    val out = Outlet[BigDecimal]("SumNodeInput.out")

    override def shape: UniformFanInShape[BigDecimal, BigDecimal] = UniformFanInShape(out, in: _*)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
      new GraphStageLogic(shape) with OutHandler {

        private var runningTotal: BigDecimal = 0
        private var receivedFrom: List[Inlet[BigDecimal]] = List.empty

        in.map { i =>
          setHandler(
            i,
            new InHandler {
              override def onPush(): Unit = {
                if (!receivedFrom.contains(i)) {
                  receivedFrom = receivedFrom :+ i
                }

                runningTotal += grab(i)

                if (receivedFrom.size == in.size) {
                  push(out, runningTotal)
                  runningTotal = 0
                  receivedFrom = List.empty
                }
              }
            })
        }

        override def onPull(): Unit =
          in.foreach(pull(_))

        setHandler(out, this)
      }
  }

  object OutputValueCollector {
    final case class OutputValueCollectorShape[T](in: Seq[Inlet[T]], out: Outlet[List[T]]) extends Shape {
      override def inlets: Seq[Inlet[_]] = in
      override def outlets: Seq[Outlet[_]] = out :: Nil
      override def deepCopy(): Shape = OutputValueCollectorShape(in.map(_.carbonCopy()), out.carbonCopy())
    }

    def apply[T](inputPorts: Int): OutputValueCollector[T] =
      new OutputValueCollector(inputPorts)
  }

  final class OutputValueCollector[T](inputPorts: Int) extends GraphStage[OutputValueCollectorShape[T]] {

    val in = Vector.tabulate(inputPorts)(i => Inlet[T]("OutputValueCollector.in" + i))
    val out = Outlet[List[T]]("OutputValueCollector.out")

    override def shape: OutputValueCollectorShape[T] = OutputValueCollectorShape(in, out)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
      new GraphStageLogic(shape) {

        private val requiredValues = in.size
        private var receivedValues: Map[Inlet[T], T] = Map.empty

        setHandler(
          out,
          new OutHandler {
            override def onPull(): Unit =
              in.foreach(pull(_))
          })
        in.foreach { i =>
          setHandler(
            i,
            new InHandler {
              override def onPush(): Unit = {
                if (!receivedValues.contains(i)) {
                  receivedValues += (i -> grab(i))
                }

                if (receivedValues.size == requiredValues) {
                  val output = receivedValues.values.toList
                  receivedValues = Map.empty
                  push(out, output)
                }
              }
            })
        }
      }
  }
}
