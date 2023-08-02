package less.stupid.neat

import scala.util.Random

trait GenomeMutator {
  def mutate(genome: Genome): Genome
}

final class NEATGenomeMutator()(implicit
    mutationConfiguration: MutationConfiguration,
    nodeIdProvider: NodeIdProvider,
    innovationNumberProvider: InnovationNumberProvider,
    geneRepository: GeneRepository)
    extends GenomeMutator {

  def mutate(genome: Genome): Genome = {
    if (mutationConfiguration.shouldAddNode()) {
      addNode(genome)
    } else if (mutationConfiguration.shouldAddLink()) {
      addLink(genome)
    } else {
      if (mutationConfiguration.shouldMutateWeights()) {
        mutateWeights(genome)
      } else if (mutationConfiguration.shouldToggleEnabled()) {
        toggleEnabled(genome)
      } else {
        genome
      }
    }
  }

  def addNode(genome: Genome): Genome = {
    if (genome.genes.isEmpty) genome
    else {
      val selectedGene = genome.genes(Random.nextInt(genome.genes.size))
      val newNode = Node.spawnHiddenNodeGene()
      val fromGene = Gene(selectedGene.from, newNode)
      val toGene = Gene(newNode, selectedGene.to)
      val newGenes = genome.genes.filterNot(_ == selectedGene) ++ List(fromGene, toGene)
      Genome(newGenes)
    }
  }

  def addLink(genome: Genome): Genome = {
    if (genome.genes.isEmpty) genome
    else {
      def findUnconnectedNodes(connections: List[(Node, Node)], nodes: List[Node]): Option[(Node, Node)] = {
        for (node1 <- nodes) {
          for (node2 <- nodes if node1.nodeId.value < node2.nodeId.value) {
            if (!connections.contains((node1, node2)) && !connections.contains((node2, node1))) {
              return Some((node1, node2))
            }
          }
        }
        None
      }

      // We want to find two nodes which aren't currently linked
      // Shuffling the list of genes avoids always finding the FIRST unlinked pair
      val shuffledGenes = Random.shuffle(genome.genes)
      val nodes = shuffledGenes.flatMap(gene => List(gene.from, gene.to))
      val connections = shuffledGenes.map(gene => (gene.from, gene.to))

      findUnconnectedNodes(connections, nodes)
        .map {
          case (from, to) => Gene(from, to)
        }
        .map(newGene => genome.copy(genes = genome.genes :+ newGene))
        .getOrElse(genome)
    }
  }

  // The body of this function is taken (almost) verbatim from the C++ reference implementation
  // written by Stanley & Miikkulainen - so blame them for all the magic numbers, etc
  // TODO: Tidy up (magic numbers -> configuration values)
  def mutateWeights(genome: Genome): Genome = {
    val severe = Random.nextBoolean()
    val numberOfGenes = genome.genes.size
    val tailEnd = numberOfGenes * 0.8
    val powerMod = 1.0
    val rate = 1.0

    val newGenes = genome.genes.zipWithIndex.map {
      case (gene, index) =>
        val (gausspoint, coldgausspoint) = if (severe) {
          (0.3, 0.1)
        } else if (numberOfGenes >= 10.0 && index > tailEnd) {
          (0.5, 0.3)
        } else {
          //Half the time don't do any cold mutations
          if (Random.nextBoolean()) {
            (1.0 - rate, 1.0 - rate - 0.1)
          } else {
            (1.0 - rate, 1.0 - rate)
          }
        }

        val randomNumber = {
          val rand = Random.nextFloat() * mutationConfiguration.weightMutationPower * powerMod
          if (Random.nextBoolean()) 0 - rand else rand
        }

        val randomChoice = Random.nextFloat()

        def pinToMaxAbsolute(num: BigDecimal, max: BigDecimal): BigDecimal = {
          if (num > max) max
          else if (num < -max) -max
          else num
        }

        val newWeight =
          if (randomChoice > gausspoint) pinToMaxAbsolute(gene.weight + randomNumber, 8)
          else if (randomChoice > coldgausspoint) pinToMaxAbsolute(randomNumber, 8)
          else gene.weight

        gene.copy(weight = newWeight)
    }

    genome.copy(genes = newGenes)
  }

  def toggleEnabled(genome: Genome): Genome = {
    val randomGene = genome.genes(Random.nextInt(genome.genes.size))

    // if we're switching from disabled -> enabled just go ahead
    if (!randomGene.enabled)
      genome.copy(genes = genome.genes.filterNot(_ == randomGene) :+ randomGene.copy(enabled = true))
    // however, if we're switching from enabled -> disabled we need to make sure there's another connection coming
    // out of the 'from' node on the connection otherwise a section of the network will break off an become isolated
    else {
      // we only want to disable hidden nodes...
      val shuffledGenes = Random.shuffle(genome.genes.collect {
        case gene if gene.from.nodeType == NodeType.Hidden => gene
      })
      for (gene <- shuffledGenes) {
        // if there's more than one connection 'from' the same node
        val maybeDisabled = genome.genes.filter(_.from == gene.from) match {
          case Nil       => None
          case _ :: Nil  => None
          case head :: _ => Some(head) // oo-er!
        }
        // then disable the first one
        if (maybeDisabled.isDefined) {
          return maybeDisabled.map { geneToDisable =>
            val disabledGene = geneToDisable.copy(enabled = false)
            val newGenes =
              (genome.genes.filterNot(_ == geneToDisable) :+ disabledGene).sortBy(_.innovationNumber.value)
            genome.copy(genes = newGenes)
          }.get
        }
      }
      genome
    }
  }
}
