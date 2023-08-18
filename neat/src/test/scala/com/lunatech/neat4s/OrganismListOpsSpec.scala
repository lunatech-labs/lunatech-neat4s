package com.lunatech.neat4s

import GenomeUtils._
import OrganismListOpsSpec._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class OrganismListOpsSpec extends AnyWordSpecLike with Matchers {

  "randomGenome" should {

    "return None if the list is empty" in {

      val genomes: List[Organism] = List.empty

      genomes.randomOrganism shouldBe None
    }

    "return the only item if there's only one item in the list" in {

      val organism = Organism.withFitness(1.0)
      val genomes: List[Organism] = List(organism)

      genomes.randomOrganism.get shouldBe organism
    }

    "never return the organism it's told to ignore" in {

      val organismToBeIgnored = Organism.withFitness(0.0)

      (0 until 100).foreach { _ =>
        val genomes: List[Organism] = List(organismToBeIgnored, Organism.withFitness(1.0))

        genomes.randomOrganism(not = organismToBeIgnored) shouldNot be(organismToBeIgnored)
      }
    }
  }

  "mate" should {

    implicit val settings = NoopMutationConfiguration
    implicit val mutator = NoopGenomeMutator
    implicit val nodeIdProvider = createNodeIdProvider()
    implicit val innovationNumberProvider = createInnovationNumberProvider()

    "return None if list is empty" in {

      val genomes: List[Organism] = List.empty

      genomes.mate() shouldBe None
    }

    "return the genome of the only organism if there's only one item in the list" in {

      val organism = Organism.withFitness(1.0)
      val genomes: List[Organism] = List(organism)

      genomes.mate() shouldBe Some(organism.genome)
    }

    "return random genes from each parent where they match, disjoint and excess from the most fit parent" in {

      val node1 = Node(NodeId(1), NodeType.Input, ActivationFunctionType.None)
      val node2 = Node(NodeId(2), NodeType.Input, ActivationFunctionType.None)
      val node3 = Node(NodeId(3), NodeType.Input, ActivationFunctionType.None)
      val node4 = Node(NodeId(4), NodeType.Output, ActivationFunctionType.None)
      val node5 = Node(NodeId(5), NodeType.Hidden, ActivationFunctionType.None)
      val node6 = Node(NodeId(6), NodeType.Hidden, ActivationFunctionType.None)

      val geneMother1 = Gene(InnovationNumber(1), node1, node4, BigDecimal(1))
      val geneMother2 = Gene(InnovationNumber(2), node2, node4, BigDecimal(1), enabled = false)
      val geneMother3 = Gene(InnovationNumber(3), node3, node4, BigDecimal(1))
      val geneMother4 = Gene(InnovationNumber(4), node2, node5, BigDecimal(1))
      val geneMother5 = Gene(InnovationNumber(5), node5, node4, BigDecimal(1), enabled = false)
      val geneMother6 = Gene(InnovationNumber(6), node5, node6, BigDecimal(1))
      val geneMother7 = Gene(InnovationNumber(7), node6, node4, BigDecimal(1))
      val geneMother9 = Gene(InnovationNumber(9), node3, node5, BigDecimal(1))
      val geneMother10 = Gene(InnovationNumber(10), node1, node6, BigDecimal(1))

      val motherGenome = Genome(
        List(
          geneMother1,
          geneMother2,
          geneMother3,
          geneMother4,
          geneMother5,
          geneMother6,
          geneMother7,
          geneMother9,
          geneMother10))
      val mother = Organism(motherGenome, 100)

      val geneFather1 = Gene(InnovationNumber(1), node1, node4, BigDecimal(1))
      val geneFather2 = Gene(InnovationNumber(2), node2, node4, BigDecimal(1), enabled = false)
      val geneFather3 = Gene(InnovationNumber(3), node3, node4, BigDecimal(1))
      val geneFather4 = Gene(InnovationNumber(4), node2, node5, BigDecimal(1))
      val geneFather5 = Gene(InnovationNumber(5), node5, node4, BigDecimal(1))
      val geneFather8 = Gene(InnovationNumber(8), node1, node5, BigDecimal(1))

      val fatherGenome = Genome(List(geneFather1, geneFather2, geneFather3, geneFather4, geneFather5, geneFather8))
      val father = Organism(fatherGenome, 1)

      val genomes: List[Organism] = List(mother, father)

      val returnedInnovationNumbers = genomes.mate().get.genes.map(_.innovationNumber)

      returnedInnovationNumbers should contain only (InnovationNumber(1), InnovationNumber(2), InnovationNumber(
        3), InnovationNumber(4), InnovationNumber(5), InnovationNumber(6), InnovationNumber(7), InnovationNumber(
        9), InnovationNumber(10))
    }

    "return random genes from each parent where they match, disjoint and excess from the most fit parent even when the fitter parent is smaller" in {

      val node1 = Node(NodeId(1), NodeType.Input, ActivationFunctionType.None)
      val node2 = Node(NodeId(2), NodeType.Input, ActivationFunctionType.None)
      val node3 = Node(NodeId(3), NodeType.Input, ActivationFunctionType.None)
      val node4 = Node(NodeId(4), NodeType.Output, ActivationFunctionType.None)
      val node5 = Node(NodeId(5), NodeType.Hidden, ActivationFunctionType.None)
      val node6 = Node(NodeId(6), NodeType.Hidden, ActivationFunctionType.None)

      val geneMother1 = Gene(InnovationNumber(1), node1, node4, BigDecimal(1))
      val geneMother2 = Gene(InnovationNumber(2), node2, node4, BigDecimal(1), enabled = false)
      val geneMother3 = Gene(InnovationNumber(3), node3, node4, BigDecimal(1))
      val geneMother4 = Gene(InnovationNumber(4), node2, node5, BigDecimal(1))
      val geneMother5 = Gene(InnovationNumber(5), node5, node4, BigDecimal(1), enabled = false)
      val geneMother6 = Gene(InnovationNumber(6), node5, node6, BigDecimal(1))
      val geneMother7 = Gene(InnovationNumber(7), node6, node4, BigDecimal(1))
      val geneMother9 = Gene(InnovationNumber(9), node3, node5, BigDecimal(1))
      val geneMother10 = Gene(InnovationNumber(10), node1, node6, BigDecimal(1))

      val motherGenome = Genome(
        List(
          geneMother1,
          geneMother2,
          geneMother3,
          geneMother4,
          geneMother5,
          geneMother6,
          geneMother7,
          geneMother9,
          geneMother10))
      val mother = Organism(motherGenome, 1)

      val geneFather1 = Gene(InnovationNumber(1), node1, node4, BigDecimal(1))
      val geneFather2 = Gene(InnovationNumber(2), node2, node4, BigDecimal(1), enabled = false)
      val geneFather3 = Gene(InnovationNumber(3), node3, node4, BigDecimal(1))
      val geneFather4 = Gene(InnovationNumber(4), node2, node5, BigDecimal(1))
      val geneFather5 = Gene(InnovationNumber(5), node5, node4, BigDecimal(1))
      val geneFather8 = Gene(InnovationNumber(8), node1, node5, BigDecimal(1))

      val fatherGenome = Genome(List(geneFather1, geneFather2, geneFather3, geneFather4, geneFather5, geneFather8))
      val father = Organism(fatherGenome, 100)

      val genomes: List[Organism] = List(mother, father)

      val returnedInnovationNumbers = genomes.mate().get.genes.map(_.innovationNumber)

      returnedInnovationNumbers should contain only (InnovationNumber(1), InnovationNumber(2), InnovationNumber(
        3), InnovationNumber(4), InnovationNumber(5), InnovationNumber(8))
    }

    "return all genes from both parents (including matching, disjoint and excess) when both genomes are of equal fitness" in {

      val node1 = Node(NodeId(1), NodeType.Input, ActivationFunctionType.None)
      val node2 = Node(NodeId(2), NodeType.Input, ActivationFunctionType.None)
      val node3 = Node(NodeId(3), NodeType.Input, ActivationFunctionType.None)
      val node4 = Node(NodeId(4), NodeType.Output, ActivationFunctionType.None)
      val node5 = Node(NodeId(5), NodeType.Hidden, ActivationFunctionType.None)
      val node6 = Node(NodeId(6), NodeType.Hidden, ActivationFunctionType.None)

      val geneMother1 = Gene(InnovationNumber(1), node1, node4, BigDecimal(1))
      val geneMother2 = Gene(InnovationNumber(2), node2, node4, BigDecimal(1), enabled = false)
      val geneMother3 = Gene(InnovationNumber(3), node3, node4, BigDecimal(1))
      val geneMother4 = Gene(InnovationNumber(4), node2, node5, BigDecimal(1))
      val geneMother5 = Gene(InnovationNumber(5), node5, node4, BigDecimal(1), enabled = false)
      val geneMother6 = Gene(InnovationNumber(6), node5, node6, BigDecimal(1))
      val geneMother7 = Gene(InnovationNumber(7), node6, node4, BigDecimal(1))
      val geneMother9 = Gene(InnovationNumber(9), node3, node5, BigDecimal(1))
      val geneMother10 = Gene(InnovationNumber(10), node1, node6, BigDecimal(1))

      val motherGenome = Genome(
        List(
          geneMother1,
          geneMother2,
          geneMother3,
          geneMother4,
          geneMother5,
          geneMother6,
          geneMother7,
          geneMother9,
          geneMother10))
      val mother = Organism(motherGenome, 1)

      val geneFather1 = Gene(InnovationNumber(1), node1, node4, BigDecimal(1))
      val geneFather2 = Gene(InnovationNumber(2), node2, node4, BigDecimal(1), enabled = false)
      val geneFather3 = Gene(InnovationNumber(3), node3, node4, BigDecimal(1))
      val geneFather4 = Gene(InnovationNumber(4), node2, node5, BigDecimal(1))
      val geneFather5 = Gene(InnovationNumber(5), node5, node4, BigDecimal(1))
      val geneFather8 = Gene(InnovationNumber(8), node1, node5, BigDecimal(1))

      val fatherGenome = Genome(List(geneFather1, geneFather2, geneFather3, geneFather4, geneFather5, geneFather8))
      val father = Organism(fatherGenome, 1)

      val genomes: List[Organism] = List(mother, father)

      val returnedInnovationNumbers = genomes.mate().get.genes.map(_.innovationNumber)

      returnedInnovationNumbers should contain only (InnovationNumber(1), InnovationNumber(2), InnovationNumber(
        3), InnovationNumber(4), InnovationNumber(5), InnovationNumber(6), InnovationNumber(7), InnovationNumber(
        8), InnovationNumber(9), InnovationNumber(10))
    }
  }
}

object OrganismListOpsSpec {

  implicit class GenomeSpecOps(self: Genome.type) {

    def empty(): Genome =
      Genome(List.empty)
  }

  implicit class OrganismSpecOps(self: Organism.type) {

    def withFitness(fitness: BigDecimal): Organism =
      Organism(Genome.empty(), fitness)
  }

  val NoopMutationConfiguration = MutationConfiguration(0, 0, 0, 0, 0)

  def createNodeIdProvider() = new AtomicNodeIdProvider()
  def createInnovationNumberProvider() = new AtomicInnovationNumberProvider()

  val NoopGenomeMutator = new GenomeMutator {
    override def mutate(genome: Genome): Genome = genome
  }
}
