package less.stupid.neat

import less.stupid.neat.NEAT.FitnessFunction

final class Population(val species: Set[Species]) {
  import Population.speciate

  def epoch(generation: Int, fitnessFunction: FitnessFunction)(implicit
      reproductionConfiguration: ReproductionConfiguration,
      mutationConfiguration: MutationConfiguration,
      speciationConfiguration: SpeciationConfiguration,
      mutator: GenomeMutator,
      nodeIdProvider: NodeIdProvider,
      innovationNumberProvider: InnovationNumberProvider,
      speciesIdProvider: SpeciesIdProvider): Population = {
    val organisms = species.flatMap(specie => specie.organisms.map(_.genome) ++ specie.genomes).map(fitnessFunction)
    val overallAverageFitness = organisms.map(_.fitness).sum / organisms.size
    val children = species.flatMap(_.breed(generation, overallAverageFitness))
    val nextSpecies = speciate(children, species)

    new Population(nextSpecies)
  }
}
object Population {

  def spawn(numberOfInputNodes: Int, numberOfOutputNodes: Int, count: Int = 100)(implicit
      reproductionConfiguration: ReproductionConfiguration,
      speciationConfiguration: SpeciationConfiguration,
      nodeIdProvider: NodeIdProvider,
      innovationNumberProvider: InnovationNumberProvider,
      speciesIdProvider: SpeciesIdProvider,
      geneRepository: GeneRepository): Population = {
    val genomes = Set.fill(count)(Genome.minimal(numberOfInputNodes, numberOfOutputNodes))
    val species = speciate(genomes, Set.empty)
    new Population(species)
  }

  private def speciate(genomes: Set[Genome], previousSpecies: Set[Species])(implicit
      reproductionConfiguration: ReproductionConfiguration,
      speciationConfiguration: SpeciationConfiguration,
      speciesIdProvider: SpeciesIdProvider): Set[Species] = {
    import SpeciesUtils.SpeciesSetOps
    genomes.foldLeft(previousSpecies) { (listOfSpecies, genome) =>
      listOfSpecies
        .findFitForGenotype(genome)
        // if we find a match for an existing species we add the child to that species
        // this knocks out the worst-performing organism for the species
        // which maintains a stable population count
        .map(species => listOfSpecies.filterNot(_ == species) + species.withGenome(genome))
        // otherwise we just create a new species
        .getOrElse(listOfSpecies + Species(genome))
    }
  }
}
