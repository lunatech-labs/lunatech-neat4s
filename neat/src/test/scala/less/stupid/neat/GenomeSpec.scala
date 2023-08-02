package less.stupid.neat

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class GenomeSpec extends AnyWordSpecLike with Matchers {

  "distanceTo" when {

    "genomes are identical" should {

      "return a distance of zero when and coefficients set to 1" in {
        implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
        implicit val nodeIdProvider = new AtomicNodeIdProvider
        implicit val speciationConfiguration = defaultSpeciationConfiguration()
        implicit val geneRepository = new InMemoryGeneRepository()

        val inputNode1 = Node.spawnInputNodeGene()
        val inputNode2 = Node.spawnInputNodeGene()
        val hiddenNode = Node.spawnHiddenNodeGene()
        val outputNode = Node.spawnOutputNodeGene()

        val gene1 = Gene(inputNode1, hiddenNode)
        val gene2 = Gene(inputNode2, outputNode)
        val gene3 = Gene(hiddenNode, outputNode)

        val leftGenome = Genome(List(gene1, gene2, gene3))
        val rightGenome = Genome(List(gene1.copy(), gene2.copy(), gene3.copy()))

        val distance = leftGenome.distanceTo(rightGenome)

        distance shouldBe 0
      }
    }

    "genomes have matching, disjoint and excess genes" should {
      "return correct distance from default speciation configuration" in {

        implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
        implicit val nodeIdProvider = new AtomicNodeIdProvider
        implicit val speciationConfiguration = defaultSpeciationConfiguration()

        val (leftGenome, rightGenome) = createGenomes()

        val distance = leftGenome.distanceTo(rightGenome)

        distance shouldBe 5
      }

      "use the mutation weight coefficient" in {

        implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
        implicit val nodeIdProvider = new AtomicNodeIdProvider
        implicit val speciationConfiguration =
          defaultSpeciationConfiguration().copy(mutationDifferenceCoefficient = 1.5)

        val (leftGenome, rightGenome) = createGenomes(weightDifferenceOfMatchingGenes = 1)

        val distance = leftGenome.distanceTo(rightGenome)

        distance shouldBe 6.5
      }

      "use the disjoint coefficient" in {

        implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
        implicit val nodeIdProvider = new AtomicNodeIdProvider
        implicit val speciationConfiguration = defaultSpeciationConfiguration().copy(disjointCoefficient = 2.5)

        val (leftGenome, rightGenome) = createGenomes()

        val distance = leftGenome.distanceTo(rightGenome)

        distance shouldBe 9.5
      }

      "use the excess coefficient" in {

        implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
        implicit val nodeIdProvider = new AtomicNodeIdProvider
        implicit val speciationConfiguration = defaultSpeciationConfiguration().copy(excessCoefficient = 3.5)

        val (leftGenome, rightGenome) = createGenomes()

        val distance = leftGenome.distanceTo(rightGenome)

        distance shouldBe 10
      }

      "not have a problem applying all coefficients at once" in {

        implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
        implicit val nodeIdProvider = new AtomicNodeIdProvider
        implicit val speciationConfiguration = SpeciationConfiguration(
          speciesCompatibilityThreshold = 0,
          disjointCoefficient = 2.5,
          excessCoefficient = 3.5,
          mutationDifferenceCoefficient = 1.5)

        val (leftGenome, rightGenome) = createGenomes(weightDifferenceOfMatchingGenes = 2)

        val distance = leftGenome.distanceTo(rightGenome)

        distance shouldBe 17.5
      }
    }

    def defaultSpeciationConfiguration(): SpeciationConfiguration =
      SpeciationConfiguration(
        speciesCompatibilityThreshold = 0,
        disjointCoefficient = 1,
        excessCoefficient = 1,
        mutationDifferenceCoefficient = 1)

    def createGenomes(weightDifferenceOfMatchingGenes: BigDecimal = 0)(implicit
        nodeIdProvider: NodeIdProvider,
        innovationNumberProvider: InnovationNumberProvider): (Genome, Genome) = {
      val inputNode1 = Node.spawnInputNodeGene()
      val inputNode2 = Node.spawnInputNodeGene()
      val hiddenNode = Node.spawnHiddenNodeGene()
      val outputNode = Node.spawnOutputNodeGene()

      val innovationNumber1 = innovationNumberProvider.next()
      val innovationNumber2 = innovationNumberProvider.next()
      val innovationNumber3 = innovationNumberProvider.next()
      val innovationNumber4 = innovationNumberProvider.next()
      val innovationNumber5 = innovationNumberProvider.next()
      val innovationNumber6 = innovationNumberProvider.next()
      val innovationNumber7 = innovationNumberProvider.next()
      val innovationNumber8 = innovationNumberProvider.next()

      val leftMatching1 = Gene(innovationNumber1, inputNode1, hiddenNode, 1)
      val leftMatching2 = Gene(innovationNumber2, hiddenNode, outputNode, 1)
      val leftMatching3 = Gene(innovationNumber3, inputNode2, outputNode, 1)

      val leftDisjointNode1 = Node.spawnHiddenNodeGene()

      val leftDisjointGene1 = Gene(innovationNumber4, inputNode1, outputNode, 1)
      val leftDisjointGene2 = Gene(innovationNumber7, inputNode1, leftDisjointNode1, 1)
      val leftDisjointGene3 = Gene(innovationNumber8, leftDisjointNode1, outputNode, 1)

      val leftGenome = Genome(
        List(leftMatching1, leftMatching2, leftMatching3, leftDisjointGene1, leftDisjointGene2, leftDisjointGene3))

      val rightMatching1 = Gene(innovationNumber1, inputNode1, hiddenNode, 1 + (weightDifferenceOfMatchingGenes / 2))
      val rightMatching2 = Gene(innovationNumber2, hiddenNode, outputNode, 1 + (weightDifferenceOfMatchingGenes / 2))
      val rightMatching3 = Gene(innovationNumber3, inputNode2, outputNode, 1)

      val rightDisjointNode1 = Node.spawnHiddenNodeGene()

      val rightExcessGene1 = Gene(innovationNumber5, inputNode2, rightDisjointNode1, 1)
      val rightExcessGene2 = Gene(innovationNumber6, rightDisjointNode1, outputNode, 1)

      val rightGenome =
        Genome(List(rightMatching3, rightMatching1, rightMatching2, rightExcessGene1, rightExcessGene2))

      (leftGenome, rightGenome)
    }
  }
}
