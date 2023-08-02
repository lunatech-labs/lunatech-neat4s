package less.stupid.neat

import less.stupid.neat.TestUtils._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class SpeciesSpec extends AnyWordSpecLike with Matchers {

  "apply" when {

    "given a genome" should {

      "create a new Species" in {

        val expectedSpeciesId = SpeciesId(100)

        implicit val speciesIdProvider = new SpeciesIdProvider {
          override def next(): SpeciesId = expectedSpeciesId
        }
        val expectedGenome = Genome(List.empty)
        val species = Species(expectedGenome)

        species.id shouldBe expectedSpeciesId
        species.organisms shouldBe empty
        species.genomes should contain only expectedGenome
      }
    }

    "given an organism" should {

      "create a new Species" in {

        val expectedSpeciesId = SpeciesId(100)

        implicit val speciesIdProvider = new SpeciesIdProvider {
          override def next(): SpeciesId = expectedSpeciesId
        }
        val expectedOrganism = Organism(Genome(List.empty), 999)
        val species = Species(expectedOrganism)

        species.id shouldBe expectedSpeciesId
        species.organisms should contain only expectedOrganism
        species.genomes shouldBe empty
      }
    }
  }

  "withGenome" should {

    "add the new genome AND remove the least fit organism" in {

      val genome = Genome(List.empty)
      val bestOrganism = Organism(genome, 1000)
      val mehOrganism = Organism(genome, 100)
      val worstOrganism = Organism(genome, 1)
      val organisms = List(mehOrganism, worstOrganism, bestOrganism)

      val species = Species(SpeciesId(101), organisms, Set.empty)
      val returnedSpecies = species.withGenome(genome)

      returnedSpecies.genomes should contain only genome
      returnedSpecies.organisms should not contain worstOrganism
    }
  }

  "breed" should {

    "produce a single child if the average fitness of the organisms in the species is equal to overall average fitness" in {

      implicit val reproductionConfiguration = NoopReproductionConfiguration
      implicit val mutationConfiguration = NoopMutationConfiguration
      implicit val mutator = NoopMutator
      implicit val nodeIdProvider = NoopNodeIdProvider
      implicit val innovationNumberProvider = NoopInnovationNumberProvider

      val emptyGenome = Genome(List.empty)
      val organism1 = Organism(emptyGenome, 100)
      val organism2 = Organism(emptyGenome, 100)
      val species = Species(SpeciesId(1), List(organism1, organism2), Set.empty)

      val speciesWithAverageFitnessOf100 = 100

      val children = species.breed(0, speciesWithAverageFitnessOf100)

      children should have size 1
    }

    "produce more than one child if the average fitness of the organisms in the species is greater than the overall average fitness" in {

      implicit val reproductionConfiguration = NoopReproductionConfiguration
      implicit val mutationConfiguration = NoopMutationConfiguration
      implicit val mutator = NoopMutator
      implicit val nodeIdProvider = NoopNodeIdProvider
      implicit val innovationNumberProvider = NoopInnovationNumberProvider

      val emptyGenome = Genome(List.empty)
      val organism1 = Organism(emptyGenome, 100)
      val organism2 = Organism(emptyGenome, 100)
      val speciesWithAverageFitnessOf100 = Species(SpeciesId(1), List(organism1, organism2), Set.empty)

      val overallAverageFitnessOf50 = 50

      val children = speciesWithAverageFitnessOf100.breed(0, overallAverageFitnessOf50)

      children should have size 2
    }

    "produce no children if the average fitness of the organisms in the species is less than the overall average fitness" in {

      implicit val reproductionConfiguration = NoopReproductionConfiguration
      implicit val mutationConfiguration = NoopMutationConfiguration
      implicit val mutator = NoopMutator
      implicit val nodeIdProvider = NoopNodeIdProvider
      implicit val innovationNumberProvider = NoopInnovationNumberProvider

      val emptyGenome = Genome(List.empty)
      val organism1 = Organism(emptyGenome, 100)
      val organism2 = Organism(emptyGenome, 100)
      val speciesWithAverageFitnessOf100 = Species(SpeciesId(1), List(organism1, organism2), Set.empty)

      val overallAverageFitnessOf101 = 101

      val children = speciesWithAverageFitnessOf100.breed(0, overallAverageFitnessOf101)

      children shouldBe empty
    }
  }
}
