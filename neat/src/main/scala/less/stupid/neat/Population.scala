package less.stupid.neat

import less.stupid.neat.SpeciesOps._

final case class Population(currentGeneration: Generation, genotypes: List[Genotype], species: List[Species]) {

  def nextGeneration(fitnessEvaluator: FitnessEvaluator)(implicit settings: EvolutionSettings): Population = {
    val nextGeneration = currentGeneration.next()
    val evaluatedGenotypes = evaluateFitness(fitnessEvaluator)
    val species = speciate(evaluatedGenotypes, nextGeneration)
    val sumOfAverageFitness = Fitness(species.map(_.averageFitness.value).sum)
    val children = species.flatMap(_.breed(nextGeneration, sumOfAverageFitness, species.size))

    Population(nextGeneration, children, species)
  }

  private def evaluateFitness(fitnessEvaluator: FitnessEvaluator)(implicit
      settings: EvolutionSettings): List[EvaluatedGenotype] =
    genotypes.map(genotype => EvaluatedGenotype(genotype, fitnessEvaluator.evaluate(genotype)))

  private def speciate(evaluatedGenotypes: List[EvaluatedGenotype], generation: Generation)(implicit
      settings: EvolutionSettings): List[Species] = {
    evaluatedGenotypes.foldLeft(species) { (listOfSpecies, evaluatedGenotype) =>
      species :+ listOfSpecies
        .findFitForGenotype(evaluatedGenotype.genotype)
        .map(_.withEvaluatedGenotype(evaluatedGenotype, generation))
        .getOrElse(Species(evaluatedGenotype, generation))
    }
  }

//  private def speciesFor(evaluatedGenotype: EvaluatedGenotype, generation: Generation)(implicit
//      settings: EvolutionSettings): Species = {
////    species
////      .findFitForGenotype(evaluatedGenotype.genotype)
////      .map(_.withEvaluatedGenotype(evaluatedGenotype, generation))
////      .getOrElse(Species(evaluatedGenotype, generation))
//  }
}

object Population {

  def apply(generation: Generation, genotypes: List[Genotype]): Population =
    new Population(generation, genotypes, List.empty)

  def initialPopulation()(implicit settings: EvolutionSettings): Population =
    Population(Generation.Zero, List.fill(settings.populationSize)(Genotype.default()), List.empty)

}
