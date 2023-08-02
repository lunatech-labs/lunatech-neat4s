package less.stupid.neat

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Await
import scala.concurrent.duration._

final class AkkaStreamsNetworkSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with Matchers
    with ScalaFutures {

  "activate" should {

    "fail if the provided input size is smaller the number of input nodes" in {
      implicit val nodeIdProvider = new AtomicNodeIdProvider()
      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val genome = Genome.minimal(2, 1)
      val network = AkkaStreamsNetworkImpl.fromGenome(genome)

      val input: List[BigDecimal] = List(1)
      val future = network.activate(input)

      whenReady(future.failed) { e =>
        e shouldBe a[IllegalArgumentException]
      }
    }

    "fail if the provided input size is larger the number of input nodes" in {
      implicit val nodeIdProvider = new AtomicNodeIdProvider()
      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val genome = Genome.minimal(2, 1)
      val network = AkkaStreamsNetworkImpl.fromGenome(genome)

      val input: List[BigDecimal] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val future = network.activate(input)

      whenReady(future.failed) { e =>
        e shouldBe a[IllegalArgumentException]
      }
    }

    "return the expected output from a minimal genome" in {

      implicit val nodeIdProvider = new AtomicNodeIdProvider()
      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val genome = Genome.minimal(2, 1)
      val network = AkkaStreamsNetworkImpl.fromGenome(genome)

      val input: List[BigDecimal] = List(1, 3)
      val future = network.activate(input)

      val result = Await.result(future, 3.seconds)
      val expected = List(4)

      result shouldBe expected
    }

    "return the expected output from a genome with hidden nodes" in {
      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
      implicit val nodeIdProvider = new AtomicNodeIdProvider
      implicit val geneRepository = new InMemoryGeneRepository()

      val inputNode1 = Node.spawnInputNodeGene()
      val inputNode2 = Node.spawnInputNodeGene()
      val hiddenNode = Node.spawnHiddenNodeGene()
      val outputNode = Node.spawnOutputNodeGene()

      val gene1 = Gene(inputNode1, hiddenNode)
      val gene2 = Gene(inputNode2, outputNode)
      val gene3 = Gene(hiddenNode, outputNode)

      val genome = Genome(List(gene1, gene2, gene3))
      val network = AkkaStreamsNetworkImpl.fromGenome(genome)

      val input: List[BigDecimal] = List(1, 3)
      val future = network.activate(input)

      val result = Await.result(future, 3.seconds)
      val expected = List(4)

      result shouldBe expected
    }

    "return the expected output from a more complex network" in {
      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
      implicit val nodeIdProvider = new AtomicNodeIdProvider
      implicit val geneRepository = new InMemoryGeneRepository()

      val inputNode1 = Node.spawnInputNodeGene()
      val inputNode2 = Node.spawnInputNodeGene()
      val inputNode3 = Node.spawnInputNodeGene()
      val inputNode4 = Node.spawnInputNodeGene()
      val inputNode5 = Node.spawnInputNodeGene()

      val hiddenNode1 = Node.spawnHiddenNodeGene()
      val hiddenNode2 = Node.spawnHiddenNodeGene()
      val hiddenNode3 = Node.spawnHiddenNodeGene()
      val hiddenNode4 = Node.spawnHiddenNodeGene()
      val hiddenNode5 = Node.spawnHiddenNodeGene()

      val outputNode1 = Node.spawnOutputNodeGene()
      val outputNode2 = Node.spawnOutputNodeGene()
      val outputNode3 = Node.spawnOutputNodeGene()

      val genome = Genome(
        List(
          Gene(inputNode1, hiddenNode1),
          Gene(inputNode2, hiddenNode1),
          Gene(inputNode3, hiddenNode2),
          Gene(inputNode4, hiddenNode2),
          Gene(inputNode4, hiddenNode4),
          Gene(inputNode5, outputNode3),
          Gene(hiddenNode1, hiddenNode3),
          Gene(hiddenNode2, hiddenNode3),
          Gene(hiddenNode2, hiddenNode5),
          Gene(hiddenNode4, outputNode3),
          Gene(hiddenNode3, outputNode1),
          Gene(hiddenNode3, hiddenNode5),
          Gene(hiddenNode5, outputNode1),
          Gene(hiddenNode5, outputNode2),
          Gene(hiddenNode5, outputNode3)))

      val network = AkkaStreamsNetworkImpl.fromGenome(genome)

      val input: List[BigDecimal] = List(1, 2, 3, 4, 5)
      val future = network.activate(input)

      val result = Await.result(future, 3.seconds)
      val expected = List(29, 17, 21)

      result shouldBe expected
    }
  }
}
