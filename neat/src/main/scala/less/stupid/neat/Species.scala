package less.stupid.neat

import java.util.concurrent.atomic.AtomicInteger

final case class SpeciesId(value: Int)

trait SpeciesIdProvider {
  def next(): SpeciesId
}

final class AtomicSpeciesIdProvider extends SpeciesIdProvider {
  private val counter = new AtomicInteger()
  def next(): SpeciesId =
    SpeciesId(counter.getAndIncrement())

}

final case class Species(id: SpeciesId, organisms: List[Organism], genomes: Set[Genome]) {

  /**
   * The most fit genome from this species is used as the "archetype" for the species.
   * That is, when calculating if a new genome belongs in this species it is compared with the genome from the fittest organism.
   */
  lazy val fittestGenome: Option[Genome] =
    organisms.sortBy(-_.fitness).map(_.genome).headOption.orElse(genomes.headOption)

  lazy val averageFitness: BigDecimal =
    organisms.map(_.fitness).sum / organisms.size

  /**
   * Used to add children to this species ready for use in the next generation.
   * We aim to keep a stable population count of the most fit genomes across the population so adding a child
   * means removing the least fit organism from this species.
   *
   * @param genome
   * @return
   */
  def withGenome(genome: Genome): Species = {
    val newOrganisms = if (organisms.isEmpty) organisms else organisms.sortBy(_.fitness).tail
    val newGenomes = genomes + genome
    copy(organisms = newOrganisms, genomes = newGenomes)
  }

  /**
   * To breed children from the population of this species we first calculate the number of offspring we expect to create
   * this is calculated by taking the average fitness of this species and comparing to the overall fitness average
   * of the entire population (of all species).
   *
   * `expectedOffspring = averageFitnessOfSpecies / averageFitnessOfEntirePopulation`
   *
   * In this way the best performing species are allowed to produce more children than the worst.
   * Any species with an average fitness less than the overall average won't produce any offspring.
   *
   * Note we don't return a new Species here! Speciation is done in the `Population.epoch` function - all the children
   * from all the species are gathered and speciated in the next step after breeding
   *
   * @param generation
   * @param overallAverageFitness
   * @param settings
   * @param mutator
   * @param nodeIdProvider
   * @param innovationNumberProvider
   * @return
   */
  def breed(generation: Int, overallAverageFitness: BigDecimal)(implicit
      reproductionConfiguration: ReproductionConfiguration,
      mutationConfiguration: MutationConfiguration,
      mutator: GenomeMutator,
      nodeIdProvider: NodeIdProvider,
      innovationNumberProvider: InnovationNumberProvider): List[Genome] = {
    import GenomeUtils.OrganismListOps

    val expectedOffspring = (averageFitness / overallAverageFitness).intValue

    (0 until expectedOffspring)
      .map { _ =>
        if (reproductionConfiguration.shouldMutateOnly()) {
          organisms.randomOrganism().map(organism => mutator.mutate(organism.genome))
        } else {
          organisms.mate()
        }
      }
      .flatten
      .toList
  }
}
object Species {

  def apply(organism: Organism)(implicit speciesIdProvider: SpeciesIdProvider): Species =
    new Species(speciesIdProvider.next(), List(organism), Set.empty)

  def apply(genome: Genome)(implicit speciesIdProvider: SpeciesIdProvider): Species =
    new Species(speciesIdProvider.next(), List.empty, Set(genome))
}

object SpeciesUtils {

  implicit class SpeciesSetOps(self: Set[Species]) {

    def findFitForGenotype(genome: Genome)(implicit speciationConfiguration: SpeciationConfiguration): Option[Species] =
      self.find(
        _.fittestGenome
          .map(_.distanceTo(genome) > speciationConfiguration.speciesCompatibilityThreshold)
          .getOrElse(false))
  }
}
