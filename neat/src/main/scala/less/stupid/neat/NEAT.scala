package less.stupid.neat

object NEAT {
  type FitnessFunction = Genome => Organism
  type ActivationFunction = BigDecimal => BigDecimal
}
