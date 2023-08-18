package com.lunatech.neat4s

import akka.NotUsed
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.SourceShape
import akka.stream.scaladsl.{GraphDSL, Merge, Sink, Source}
import com.lunatech.neat4s.akkastreams.NetworkNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Await
import scala.concurrent.duration._

final class NetworkNodeSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  "createInputNode" should {

    def createInputNode(): Source[BigDecimal, NotUsed] =
      Source.fromGraph(GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        val source = b.add(Source.single[BigDecimal](99))
        val inputNode = b.add(NetworkNode.createInputNode(3, ActivationFunction.noop))
        val merge = b.add(Merge[BigDecimal](3))

        source.out ~> inputNode.in
        inputNode.outlets(0) ~> merge.in(0)
        inputNode.outlets(1) ~> merge.in(1)
        inputNode.outlets(2) ~> merge.in(2)

        SourceShape(merge.out)
      })

    "output the provided input to all outlets" in {
      val source = createInputNode()
      val sink = Sink.collection[BigDecimal, List[BigDecimal]]

      val expectedResult = List(99, 99, 99)
      val future = source.runWith(sink)
      val receivedResult = Await.result(future, 3.seconds)

      receivedResult shouldBe expectedResult
    }
  }

  "createOutputNode" should {

    def createOutputNode(inputs: List[BigDecimal]): Source[BigDecimal, NotUsed] =
      Source.fromGraph(GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        val outputNode = b.add(NetworkNode.createOutputNode(inputs.size, ActivationFunction.noop))

        inputs.zipWithIndex.foreach {
          case (input, i) => b.add(Source.single(input)).out ~> outputNode.in(i)
        }

        SourceShape(outputNode.out)
      })

    "output the sum of the provided inputs to the outlet" in {
      val source = createOutputNode(List(3, 4, 5))
      val sink = Sink.head[BigDecimal]

      val expectedResult = 12
      val receivedResult = Await.result(source.runWith(sink), 3.seconds)

      receivedResult shouldBe expectedResult
    }
  }

  "createHiddenNode" should {

    def createHiddenNode(inputs: List[BigDecimal], outputPorts: Int): Source[BigDecimal, NotUsed] =
      Source.fromGraph(GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        val hiddenNode = b.add(NetworkNode.createHiddenNode(inputs.size, outputPorts, ActivationFunction.noop))

        inputs.zipWithIndex.foreach {
          case (input, i) => b.add(Source.single(input)).out ~> hiddenNode.in(i)
        }

        val merge = b.add(Merge[BigDecimal](outputPorts))

        (0 until outputPorts).foreach { i =>
          hiddenNode.out(i) ~> merge.in(i)
        }

        SourceShape(merge.out)
      })

    "output the sum of the provided inputs to all outlets" in {
      val source = createHiddenNode(List(3, 4, 5), 10)
      val sink = Sink.collection[BigDecimal, List[BigDecimal]]

      val expectedResult = List.fill(10)(12)
      val receivedResult = Await.result(source.runWith(sink), 3.seconds)

      receivedResult shouldBe expectedResult
    }
  }
}
