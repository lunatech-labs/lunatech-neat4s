package com.lunatech.neat4s

import GenomeComparison.{DisjointGenes, ExcessGenes, MatchingGenes}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class GenomeComparisonSpec extends AnyWordSpecLike with Matchers {

  "fromGenomes" should {

    "return correct value if there are no disjoint or excess genes" in {

      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
      implicit val nodeIdProvider = new AtomicNodeIdProvider

      val inputNode1 = Node.spawnInputNodeGene()
      val inputNode2 = Node.spawnInputNodeGene()
      val hiddenNode = Node.spawnHiddenNodeGene()
      val outputNode = Node.spawnOutputNodeGene()

      val innovationNumber1 = innovationNumberProvider.next()
      val innovationNumber2 = innovationNumberProvider.next()
      val innovationNumber3 = innovationNumberProvider.next()

      val leftMatching1 = Gene(innovationNumber1, inputNode1, hiddenNode, 1)
      val leftMatching2 = Gene(innovationNumber2, hiddenNode, outputNode, 1)
      val leftMatching3 = Gene(innovationNumber3, inputNode2, outputNode, 1)

      val leftGenome = Genome(List(leftMatching1, leftMatching2, leftMatching3))

      val rightMatching1 = Gene(innovationNumber1, inputNode1, hiddenNode, 1)
      val rightMatching2 = Gene(innovationNumber2, hiddenNode, outputNode, 1)
      val rightMatching3 = Gene(innovationNumber3, inputNode2, outputNode, 1)

      val rightGenome = Genome(List(rightMatching3, rightMatching1, rightMatching2))

      val comparison = GenomeComparison.fromGenomes(leftGenome, rightGenome)

      val matchingGenes = MatchingGenes(
        List((leftMatching1, rightMatching1), (leftMatching2, rightMatching2), (leftMatching3, rightMatching3)))
      val disjointGenes = DisjointGenes.empty
      val excessGenes = ExcessGenes.empty
      val expectedComparison = GenomeComparison(matchingGenes, disjointGenes, excessGenes)

      comparison shouldBe expectedComparison
    }

    "return correct value if there are disjoint genes only on one genome AND excess genes" in {

      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
      implicit val nodeIdProvider = new AtomicNodeIdProvider

      implicit val speciationConfiguration = SpeciationConfiguration(
        speciesCompatibilityThreshold = 0,
        disjointCoefficient = 1,
        excessCoefficient = 1,
        mutationDifferenceCoefficient = 1)

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

      val leftMatching1 = Gene(innovationNumber1, inputNode1, hiddenNode, 1)
      val leftMatching2 = Gene(innovationNumber2, hiddenNode, outputNode, 1)
      val leftMatching3 = Gene(innovationNumber3, inputNode2, outputNode, 1)

      val leftDisjointNode1 = Node.spawnHiddenNodeGene()

      val leftDisjointGene1 = Gene(innovationNumber4, inputNode1, leftDisjointNode1, 1)
      val leftDisjointGene2 = Gene(innovationNumber5, leftDisjointNode1, outputNode, 1)

      val leftGenome = Genome(List(leftMatching1, leftMatching2, leftMatching3, leftDisjointGene1, leftDisjointGene2))

      val rightMatching1 = Gene(innovationNumber1, inputNode1, hiddenNode, 1)
      val rightMatching2 = Gene(innovationNumber2, hiddenNode, outputNode, 1)
      val rightMatching3 = Gene(innovationNumber3, inputNode2, outputNode, 1)

      val rightDisjointNode1 = Node.spawnHiddenNodeGene()

      val rightExcessGene1 = Gene(innovationNumber6, inputNode2, rightDisjointNode1, 1)
      val rightExcessGene2 = Gene(innovationNumber7, rightDisjointNode1, outputNode, 1)

      val rightGenome =
        Genome(List(rightMatching3, rightMatching1, rightMatching2, rightExcessGene1, rightExcessGene2))

      val matchingGenes = MatchingGenes(
        List((leftMatching1, rightMatching1), (leftMatching2, rightMatching2), (leftMatching3, rightMatching3)))
      val disjointGenes = DisjointGenes(List((Some(leftDisjointGene1), None), (Some(leftDisjointGene2), None)))
      val excessGenes = ExcessGenes(List(), List(rightExcessGene1, rightExcessGene2))
      val expectedComparison = GenomeComparison(matchingGenes, disjointGenes, excessGenes)

      val comparison = GenomeComparison.fromGenomes(leftGenome, rightGenome)

      // 3 Matching genes + 2 Disjoint genes + 2 Excess genes
      comparison shouldBe expectedComparison
    }

    "return correct value if there are disjoint genes on both genomes AND excess genes" in {

      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
      implicit val nodeIdProvider = new AtomicNodeIdProvider

      implicit val speciationConfiguration = SpeciationConfiguration(
        speciesCompatibilityThreshold = 0,
        disjointCoefficient = 1,
        excessCoefficient = 1,
        mutationDifferenceCoefficient = 1)

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
      val leftExcessGene2 = Gene(innovationNumber7, inputNode1, leftDisjointNode1, 1)
      val leftExcessGene3 = Gene(innovationNumber8, leftDisjointNode1, outputNode, 1)

      val leftGenome =
        Genome(List(leftMatching1, leftMatching2, leftMatching3, leftDisjointGene1, leftExcessGene2, leftExcessGene3))

      val rightMatching1 = Gene(innovationNumber1, inputNode1, hiddenNode, 1)
      val rightMatching2 = Gene(innovationNumber2, hiddenNode, outputNode, 1)
      val rightMatching3 = Gene(innovationNumber3, inputNode2, outputNode, 1)

      val rightDisjointNode1 = Node.spawnHiddenNodeGene()

      val rightDisjointGene1 = Gene(innovationNumber5, inputNode2, rightDisjointNode1, 1)
      val rightDisjointGene2 = Gene(innovationNumber6, rightDisjointNode1, outputNode, 1)

      val rightGenome =
        Genome(List(rightMatching3, rightMatching1, rightMatching2, rightDisjointGene1, rightDisjointGene2))

      val matchingGenes = MatchingGenes(
        List((leftMatching1, rightMatching1), (leftMatching2, rightMatching2), (leftMatching3, rightMatching3)))
      val disjointGenes = DisjointGenes(
        List((Some(leftDisjointGene1), None), (None, Some(rightDisjointGene1)), (None, Some(rightDisjointGene2))))
      val excessGenes = ExcessGenes(List(leftExcessGene2, leftExcessGene3), List.empty)
      val expectedComparison = GenomeComparison(matchingGenes, disjointGenes, excessGenes)

      val comparison = GenomeComparison.fromGenomes(leftGenome, rightGenome)

      // 3 Matching genes + 3 Disjoint genes + 2 Excess genes
      comparison shouldBe expectedComparison
    }
  }
}
