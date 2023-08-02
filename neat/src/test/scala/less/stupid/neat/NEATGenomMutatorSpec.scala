package less.stupid.neat

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class NEATGenomMutatorSpec extends AnyWordSpecLike with Matchers {

  private def createNodeIdProvider(): NodeIdProvider =
    new AtomicNodeIdProvider()

  private def createInnovationNumberProvider(): InnovationNumberProvider =
    new AtomicInnovationNumberProvider()

  private def createSimpleGenome()(implicit
      nodeIdProvider: NodeIdProvider,
      innovationNumberProvider: InnovationNumberProvider,
      geneRepository: GeneRepository): Genome = {
    val node1 = Node.spawnInputNodeGene()
    val node2 = Node.spawnInputNodeGene()
    val node3 = Node.spawnOutputNodeGene()

    val gene1 = Gene(node1, node3)
    val gene2 = Gene(node2, node3)

    Genome(List(gene1, gene2))
  }

  private def createComplexGenome()(implicit
      nodeIdProvider: NodeIdProvider,
      innovationNumberProvider: InnovationNumberProvider,
      geneRepository: GeneRepository): Genome = {
    val node1 = Node.spawnInputNodeGene()
    val node2 = Node.spawnInputNodeGene()
    val node3 = Node.spawnInputNodeGene()
    val node4 = Node.spawnOutputNodeGene()
    val node5 = Node.spawnHiddenNodeGene()
    val node6 = Node.spawnHiddenNodeGene()

    val gene1 = Gene(node1, node4)
    val gene2 = Gene(node1, node6)
    val gene3 = Gene(node2, node5)
    val gene4 = Gene(node3, node4)
    val gene5 = Gene(node3, node5)
    val gene6 = Gene(node5, node6)
    val gene7 = Gene(node6, node4)

    Genome(List(gene1, gene2, gene3, gene4, gene5, gene6, gene7))
  }

  "mutate" should {

    "return the unchanged genome when all settings return false" in {
      implicit val mutationConfiguration = MutationConfiguration(0, 0, 0, 0, 0)
      implicit val nodeIdProvider = createNodeIdProvider()
      implicit val innovationNumberProvider = createInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val mutator = new NEATGenomeMutator()

      val originalGenome = createSimpleGenome()
      val mutatedGenome = mutator.mutate(originalGenome)

      mutatedGenome shouldBe originalGenome
    }

    "return a new genome with an extra node when add node setting returns true" in {
      implicit val mutationConfiguration = MutationConfiguration(1, 0, 0, 0, 0)
      implicit val nodeIdProvider = createNodeIdProvider()
      implicit val innovationNumberProvider = createInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val mutator = new NEATGenomeMutator()

      val originalGenome = createSimpleGenome()
      val mutatedGenome = mutator.mutate(originalGenome)

      mutatedGenome.genes should have size originalGenome.genes.size + 1
    }

    "return a new genome with an extra link when add node setting returns true" in {
      implicit val mutationConfiguration = MutationConfiguration(0, 1, 0, 0, 0)
      implicit val nodeIdProvider = createNodeIdProvider()
      implicit val innovationNumberProvider = createInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val mutator = new NEATGenomeMutator()

      val originalGenome = createComplexGenome()
      val mutatedGenome = mutator.mutate(originalGenome)

      mutatedGenome.genes should have size originalGenome.genes.size + 1
    }

    "return a new genome with different weights on each connection" in {
      implicit val mutationConfiguration = MutationConfiguration(0, 0, 1, 0, 0)
      implicit val nodeIdProvider = createNodeIdProvider()
      implicit val innovationNumberProvider = createInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val mutator = new NEATGenomeMutator()

      val originalGenome = createSimpleGenome()
      val originalWeights = originalGenome.genes.map(_.weight)

      val mutatedGenome = mutator.mutate(originalGenome)
      val mutatedWeights = mutatedGenome.genes.map(_.weight)

      mutatedWeights should not be originalWeights
    }

    "return a new genome with no genes disabled if there are only single connections between nodes" in {
      implicit val mutationConfiguration = MutationConfiguration(0, 0, 0, 1, 0)
      implicit val nodeIdProvider = createNodeIdProvider()
      implicit val innovationNumberProvider = createInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val mutator = new NEATGenomeMutator()

      val originalGenome = createSimpleGenome()
      val mutatedGenome = mutator.mutate(originalGenome)

      mutatedGenome shouldBe originalGenome
    }

    "return a new genome with the hidden node disabled when that is the only hidden node with two outbound connections" in {
      implicit val mutationConfiguration = MutationConfiguration(0, 0, 0, 1, 2.5)
      implicit val nodeIdProvider = createNodeIdProvider()
      implicit val innovationNumberProvider = createInnovationNumberProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val mutator = new NEATGenomeMutator()

      val firstInputNode = Node.spawnInputNodeGene()
      val secondInputNode = Node.spawnInputNodeGene()
      val hiddenNode = Node.spawnHiddenNodeGene()
      val firstOutputNode = Node.spawnOutputNodeGene()
      val secondOutputNode = Node.spawnOutputNodeGene()

      val gene1 = Gene(firstInputNode, firstOutputNode)
      val gene2 = Gene(firstInputNode, hiddenNode)
      val gene3 = Gene(secondInputNode, secondOutputNode)
      val gene4 = Gene(secondInputNode, hiddenNode)
      val gene5 = Gene(hiddenNode, firstOutputNode)
      val gene6 = Gene(hiddenNode, secondOutputNode)

      val originalGenome = Genome(List(gene1, gene2, gene3, gene4, gene5, gene6))
      val expectedGenome = Genome(List(gene1, gene2, gene3, gene4, gene5.copy(enabled = false), gene6))

      val mutatedGenome = mutator.mutate(originalGenome)

      mutatedGenome shouldBe expectedGenome
    }
  }
}
