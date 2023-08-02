package less.stupid.neat

import scala.util.Random

object GenomeUtils {

  implicit class OrganismListOps(self: List[Organism]) {

    def randomOrganism(not: Organism): Option[Organism] =
      self.filterNot(_ == not).randomOrganism()

    def randomOrganism(): Option[Organism] =
      if (self.isEmpty) None
      else Some(self(Random.nextInt(self.size)))

    def mate()(implicit
        settings: MutationConfiguration,
        mutator: GenomeMutator,
        nodeIdProvider: NodeIdProvider,
        innovationNumberProvider: InnovationNumberProvider): Option[Genome] =
      self match {
        case Nil         => None
        case head :: Nil => Some(mutator.mutate(head.genome))
        case _ =>
          for {
            mother <- self.randomOrganism()
            father <- self.randomOrganism(not = mother)
          } yield {

            val (leastFitParent, mostFitParent) =
              if (mother.fitness < father.fitness) (mother, father) else (father, mother)

            val comparison = GenomeComparison.fromGenomes(leastFitParent.genome, mostFitParent.genome)

            // If both parents are of equal fitness
            // We return ALL the genes from both parents
            // When both parents have a gene with the same innovation number we select one randomly
            val inheritedGenes = if (mother.fitness == father.fitness) {
              comparison.matching.random() ++ comparison.disjoint.all ++ comparison.excess.all
            } else {
              // If one parent is more fit than the other
              // If both parents have a gene with the same innovation number we select one randomly
              // Excess and Disjoint genes are returned from the parent which is fitter
              comparison.matching.random() ++ comparison.disjoint.right ++ comparison.excess.right
            }

            Genome(inheritedGenes)
          }
      }
  }

}
