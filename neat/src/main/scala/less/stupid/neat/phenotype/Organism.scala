package less.stupid.neat.phenotype

import less.stupid.neat.genotype.Genome

final case class Organism(genome: Genome, phenome: Network, fitness: BigDecimal)
