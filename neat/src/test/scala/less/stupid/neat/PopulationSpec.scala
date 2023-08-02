package less.stupid.neat

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Random

final class PopulationSpec extends AnyWordSpecLike with Matchers {

  "create" should {
    "create an population with the correct default settings" in {
      implicit val settings = EvolutionSettings()
      val expectedGenotypeCount = settings.populationSize

      val population = Population.initialPopulation()

      population.currentGeneration shouldBe Generation(0)
      population.genotypes should have size expectedGenotypeCount
      population.species shouldBe List.empty
    }
  }

  "nextGeneration" should {
    "create a new generation of children based on the fittest members of the current population" in {
      implicit val settings = EvolutionSettings()
      val initialPopulation = Population.initialPopulation()

      val fitnessEvaluator = new FitnessEvaluator {
        override def evaluate(genotype: Genotype): Fitness = Fitness(new Random().nextDouble())
      }

      val nextGeneration = initialPopulation.nextGeneration(fitnessEvaluator)

      println(nextGeneration)
    }
  }
}
