package less.stupid.neat

import akka.NotUsed
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.SourceShape
import akka.stream.scaladsl.{GraphDSL, Merge, Sink, Source}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Await
import scala.concurrent.duration._

final class MapListElementsToOutletsSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with Matchers
    with ScalaFutures {

  "apply" should {

    def createSource(input: List[BigDecimal]): Source[BigDecimal, NotUsed] =
      Source.fromGraph(GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        val source = b.add(Source.single[List[BigDecimal]](input))
        val mapper = b.add(MapListElementsToOutputs[BigDecimal](outputPorts = 5))
        val merge = b.add(Merge[BigDecimal](5))

        source.out ~> mapper.in
        (0 until 5).foreach { i =>
          mapper.out(i) ~> merge.in(i)
        }

        SourceShape(merge.out)
      })

    "output the elements of the supplied list to outlets" in {

      val expectedResult: List[BigDecimal] = List(1, 2, 3, 4, 5)
      val source = createSource(expectedResult)

      val sink = Sink.collection[BigDecimal, List[BigDecimal]]

      val resultReceived = Await.result(source.runWith(sink), 3.seconds)

      resultReceived shouldBe expectedResult
    }

    "fail if the size of the input list does not match the number of outlets" in {
      val failingInput: List[BigDecimal] = List(1, 2)
      val source = createSource(failingInput)
      val sink = Sink.ignore

      whenReady(source.runWith(sink).failed) { e =>
        e shouldBe a[IllegalArgumentException]
      }
    }
  }
}
