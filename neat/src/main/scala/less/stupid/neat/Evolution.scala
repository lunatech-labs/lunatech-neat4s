package less.stupid.neat

import less.stupid.neat.Fitness.Zero

final case class RunId(value: Int)

//final case class NumberOfGenerations(value: Int)
//final case class GenerationSize(value: Int)

final case class EvolutionSettings(
    generations: Int = 2000,
    populationSize: Int = 100,
    defaultWeightRange: BigDecimal = 4.5,
    MUTATE_ADD_NODE: BigDecimal = 0.07,
    MUTATE_ADD_CONNECTION: BigDecimal = 0.15,
    MUTATE_ENABLE_DISABLE: BigDecimal = 0.4, // 0
    MUTATE_WEIGHT: BigDecimal = 0.8, // the chance of mutating connection weights //0.8
    MUTATE_WEIGHT_SMALL: BigDecimal = 0.9, // if the connections are to be changed, this decides the small/random ratio
    MUTATE_SINGLE_INSTEAD: BigDecimal = 0.1, // chance of mutating only a single weight
    MUTATE_FUNCTION: BigDecimal = 0.15,
    MUTATE_SMALL_LIMIT: BigDecimal = 0.05, // 0.05
    COMPAT_1: BigDecimal = 1.5,
    COMPAT_2: BigDecimal = 1.5,
    DELTA_T: BigDecimal = 3.0,
    CROSSOVER: BigDecimal = 0.75,
    KILL_OFF: BigDecimal = 0.5,
    INPUT_NODES: Int = 5, // x1, y1, x2, y2, bias
    OUTPUT_NODES: Int = 2 // weights input->hidden and hidden->output
)

final case class Generation(value: Int) {

  def next(): Generation =
    Generation(value + 1)

  def <>(other: Generation): Int =
    value - other.value
}

object Generation {
  val Zero = Generation(0)
}

final case class Fitness(value: BigDecimal) {
  def +(other: Fitness): Fitness = Fitness(value + other.value)
  def >(other: Fitness): Boolean = value > other.value
  def /(other: Fitness): Fitness = Fitness(value / other.value)
  def *(other: Int): Int = (value * other).intValue
  def isZero(): Boolean = this == Zero
}
object Fitness {
  val Zero = Fitness(BigDecimal(0))
}

class Evolution(runId: RunId, fitnessEvaluator: FitnessEvaluator)(implicit settings: EvolutionSettings) {

  def run(population: Population) = {

    val evaluatedGenotypes = population.nextGeneration(fitnessEvaluator)

//    // Calculate Fitness
//    val genotypesAndFitnesses = population.genotypes.map { genotype =>
//      val fitness = fitnessEvaluator.evaluate(genotype)
//      (genotype, fitness)
//    }
//
//    // Speciate
//    val newSpecies = genotypesAndFitnesses.map { genotypeAndFitness =>
//      // For each genotype
//      // check if it's distance to the archetypal genome for any existing species is less than the threshold setting
//      // if not we have a new species and this genome is the archetype
//      val (genotype, fitness) = genotypeAndFitness
//      val species = population.speciesFor(genotype)
//      species
//    }.filterNot(_.genotypes.isEmpty)
//
//    // Breed
//    val sumOfAverageFitnessOfAllSpecies = newSpecies.map(_.averageFitness).sum
//
//
//    // Mutate

  }
}
