package com.lunatech.neat4s

object TestUtils {

  val NoopReproductionConfiguration: ReproductionConfiguration = ReproductionConfiguration(1)
  val NoopSpeciationConfiguration: SpeciationConfiguration = SpeciationConfiguration(0, 1, 1, 1)
  val NoopMutationConfiguration: MutationConfiguration = MutationConfiguration(0, 0, 0, 0, 0)

  val NoopMutator: GenomeMutator = new GenomeMutator {
    override def mutate(genome: Genome): Genome = genome
  }

  val NoopNodeIdProvider: NodeIdProvider = () => NodeId(0)

  val NoopInnovationNumberProvider: InnovationNumberProvider = () => InnovationNumber(0)
}
