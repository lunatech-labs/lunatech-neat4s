package less.stupid.neat

final case class Genome(genes: List[Gene]) {

  /**
   * This function gives a measure of compatibility between two Genomes by computing a linear combination of 3
   * characterizing variables of their compatibility.
   *
   * The 3 variables represent
   *
   * - PERCENT DISJOINT GENES
   * - PERCENT EXCESS GENES
   * - MUTATIONAL DIFFERENCE WITHIN MATCHING GENES
   *
   * The formula for compatibility is:
   *
   * `disjoint_coeff * pdg + excess_coeff * peg + mutdiff_coeff * mdmg`
   *
   * (The 3 coefficients are global system parameters from configuration)
   *
   * @param other
   * @return
   */
  def distanceTo(other: Genome)(implicit speciationConfiguration: SpeciationConfiguration): BigDecimal = {
    val comparison = GenomeComparison.fromGenomes(this, other)

    (comparison.matching.weightDifference * speciationConfiguration.mutationDifferenceCoefficient) +
    (speciationConfiguration.disjointCoefficient * comparison.disjoint.size) +
    (speciationConfiguration.excessCoefficient * comparison.excess.size)
  }
}
object Genome {

  def minimal(inputCount: Int, outputCount: Int)(implicit
      nodeIdProvider: NodeIdProvider,
      innovationNumberProvider: InnovationNumberProvider,
      geneRepository: GeneRepository): Genome = {
    val inputNodes = List.fill(inputCount)(Node.spawnInputNodeGene())
    val outputNodes = List.fill(outputCount)(Node.spawnOutputNodeGene())
    val genes = inputNodes.flatMap { inputNode =>
      outputNodes.map { outputNode =>
        Gene(inputNode, outputNode)
      }
    }
    Genome(genes)
  }
}
