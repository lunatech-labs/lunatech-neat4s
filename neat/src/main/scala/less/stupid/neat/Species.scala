package less.stupid.neat

import java.util.UUID
import less.stupid.neat.Mutations._

final case class SpeciesId(value: UUID)
object SpeciesId {
  def random(): SpeciesId =
    new SpeciesId(UUID.randomUUID())
}

final case class Species(
    id: SpeciesId,
    lastInnovation: Generation,
    archetype: Genotype,
    evaluatedGenotypes: List[EvaluatedGenotype],
    bestGenotype: Genotype,
    bestFitness: Fitness,
    averageFitness: Fitness) {

  /**
   * Adds a new evaluated Genotype to this species.
   *
   * This will return a new species, keeping track of the last innovation (the last generation when this species
   * improved its best fitness) as well as the references to best genotype, best fitness and the updated average
   * fitness of the species
   *
   * These properties are used when breeding from this species.
   *
   * @param evaluatedGenotype
   * @return
   *   A new Species with updated properties (see above)
   */
  def withEvaluatedGenotype(evaluatedGenotype: EvaluatedGenotype, generation: Generation): Species = {
    val (nextInnovation, nextBestGenotype, nextBestFitness) = if (evaluatedGenotype.fitness > bestFitness) {
      (generation, evaluatedGenotype.genotype, evaluatedGenotype.fitness)
    } else {
      (lastInnovation, bestGenotype, bestFitness)
    }
    val nextEvaluatedGenotypes = evaluatedGenotypes :+ evaluatedGenotype
    val nextAverageFitness = {
      val total = evaluatedGenotypes.map(_.fitness.value).sum
      Fitness(total / evaluatedGenotypes.size)
    }
    copy(
      lastInnovation = nextInnovation,
      evaluatedGenotypes = nextEvaluatedGenotypes,
      bestGenotype = nextBestGenotype,
      bestFitness = nextBestFitness,
      averageFitness = nextAverageFitness)
  }

  def breed(generation: Generation, sumOfAllSpeciesFitness: Fitness, numberOfSpecies: Int)(implicit
      settings: EvolutionSettings): Set[Genotype] = {
    // If we haven't improved in the last 10 generations...
    if (generation <> lastInnovation > 10) {
      Set(bestGenotype)
    } else {
      val numberOfChildrenToCreate: Int = if (sumOfAllSpeciesFitness.isZero()) {
        settings.populationSize / numberOfSpecies
      } else {
        (averageFitness / sumOfAllSpeciesFitness * settings.populationSize) - 1
      }

      val numberOfGenotypesToKeep = Math.max((evaluatedGenotypes.size * (1 - settings.KILL_OFF)).intValue, 1)

      val children =
        evaluatedGenotypes.sortBy(_.fitness.value).reverse.map(_.genotype).take(numberOfGenotypesToKeep).map(_.mutate())

      children.toSet
    }
  }

  //  def evaluateFitness(fitnessEvaluator: FitnessEvaluator): SpeciesFitness = {
//    val genotypesAndFitness = genotypes.map(genotype => (genotype, fitnessEvaluator.evaluate(genotype)))
//    new SpeciesFitness(genotypesAndFitness)
//  }
}

//final class SpeciesFitness(genotypesAndFitness: List[(Genotype, Fitness)]) {
//  lazy val averageFitness: Fitness = {
//    val total = genotypesAndFitness.map {
//      case (_, fitness) => fitness.value
//    }.sum
//    Fitness(total / genotypesAndFitness.size)
//  }
//
//  lazy val bestFitness: Fitness = {
//    val max = genotypesAndFitness.map {
//      case (_, fitness) => fitness.value
//    }.max
//    Fitness(max)
//  }
//}

object Species {

  def apply(evaluatedGenotype: EvaluatedGenotype, generation: Generation): Species =
    new Species(
      SpeciesId.random(),
      generation,
      evaluatedGenotype.genotype,
      List(evaluatedGenotype),
      evaluatedGenotype.genotype,
      evaluatedGenotype.fitness,
      evaluatedGenotype.fitness)
}

object SpeciesOps {

  implicit class ListOfSpecies(self: List[Species]) {

    def findFitForGenotype(genotype: Genotype)(implicit evolutionSettings: EvolutionSettings): Option[Species] =
      self.find(species => genotype.distanceTo(species.archetype) > evolutionSettings.DELTA_T)
  }
}
