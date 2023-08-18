package com.lunatech.neat4s

object NEAT {
  type FitnessFunction = Genome => Organism
  type ActivationFunction = BigDecimal => BigDecimal
}
