package less.stupid.neat.genotype

import enumeratum.{Enum, EnumEntry}
import less.stupid.neat.ActivationFunctionType
import less.stupid.neat.genotype.Gene.Node
import less.stupid.neat.genotype.Gene.Node.NodeId
import less.stupid.neat.genotype.GenomeUtils.GenomeMutator
import less.stupid.neat.phenotype.Organism

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

sealed trait ActivationFunction extends Function[BigDecimal, BigDecimal]
object ActivationFunction {
  val noop: ActivationFunction = new ActivationFunction {
    override def apply(input: BigDecimal): BigDecimal = input
  }
}

sealed trait ActivationFunctionType extends EnumEntry
object ActivationFunctionType extends Enum[ActivationFunctionType] {
  override def values: IndexedSeq[ActivationFunctionType] = findValues

  case object Sigmoid extends ActivationFunctionType
  case object Sine extends ActivationFunctionType
  case object Cosine extends ActivationFunctionType
  case object Linear extends ActivationFunctionType
  case object Absolute extends ActivationFunctionType
  case object Gaussian extends ActivationFunctionType
  case object None extends ActivationFunctionType
}

sealed trait FitnessFunction {
  def evaluate(genome: Genome): Organism = ???
}

final case class Gene(
    innovationNumber: InnovationNumber,
    from: Node,
    to: Node,
    weight: BigDecimal,
    enabled: Boolean = true)
object Gene {
  sealed trait Node {
    val nodeId: NodeId
    val activationFunctionType: ActivationFunctionType
  }
  object Node {

    final case class NodeId(value: Int)
    object NodeId {
      private val count = new AtomicInteger(0)

      def next(): NodeId =
        NodeId(count.getAndIncrement())
    }

    final case class InputNodeGene(nodeId: NodeId, activationFunctionType: ActivationFunctionType) extends Node
    final case class BiasNodeGene(nodeId: NodeId, activationFunctionType: ActivationFunctionType) extends Node
    final case class HiddenNodeGene(nodeId: NodeId, activationFunctionType: ActivationFunctionType) extends Node
    final case class OutputNodeGene(nodeId: NodeId, activationFunctionType: ActivationFunctionType) extends Node

    def spawnInputNodeGene(): Node = {
      InputNodeGene(NodeId.next(), ActivationFunctionType.None)
    }

    def spawnBiasNodeGene(): Node = {
      BiasNodeGene(NodeId.next(), ActivationFunctionType.None)
    }

    def spawnHiddenNodeGene(): Node = {
      HiddenNodeGene(NodeId.next(), ActivationFunctionType.None)
    }

    def spawnOutputNodeGene(): Node = {
      OutputNodeGene(NodeId.next(), ActivationFunctionType.None)
    }
  }

  def apply(from: Node, to: Node): Gene = {
    new Gene(InnovationNumber.next(), from, to, BigDecimal(1))
  }
}
//sealed trait NodeGene
//trait IncomingNodeGene
//trait OutgoingNodeGene
//final case class BiasNodeGene(innovationNumber: Int) extends NodeGene with IncomingNodeGene
//final case class InputNodeGene(innovationNumber: Int) extends NodeGene with IncomingNodeGene
//final case class HiddenNodeGene(innovationNumber: Int) extends NodeGene with IncomingNodeGene with OutgoingNodeGene
//final case class OutputNodeGene(innovationNumber: Int) extends NodeGene with OutgoingNodeGene
//final case class ConnectionGene(innovationNumber: Int, from: IncomingNodeGene, to:OutgoingNodeGene)

final case class Genome(genes: Set[Gene])
object Genome {

  def minimal(inputCount: Int, outputCount: Int): Genome = {
    val inputNodes = Set.fill(inputCount)(Node.spawnInputNodeGene())
    val outputNodes = Set.fill(outputCount)(Node.spawnOutputNodeGene())
    val genes = inputNodes.flatMap { inputNode =>
      outputNodes.map { outputNode =>
        Gene(inputNode, outputNode)
      }
    }
    Genome(genes)
  }
}

object GenomeUtils {

  implicit class OrganismListOps(self: List[Organism]) {

    def randomOrganism(not: Organism): Option[Organism] =
      self.filterNot(_ == not).randomOrganism()

    def randomOrganism(): Option[Organism] =
      if (self.isEmpty) None
      else Some(self(Random.nextInt(self.size)))

    def mate()(implicit settings: MutationSettings, mutator: GenomeMutator): Option[Genome] = {
      if (self.isEmpty) None
      else if (self.size == 1) self.headOption.map(organism => mutator.mutate(organism.genome))
      else {
        for {
          mother <- self.randomOrganism()
          father <- self.randomOrganism(not = mother)
        } yield {
          val (mostFitParent, leastFitParent) =
            if (mother.fitness > father.fitness) (mother, father) else (father, mother)

          val newMatchingGenes = mostFitParent.genome.genes
            .map { geneFromMostFit =>
              leastFitParent.genome.genes
                .find(_.innovationNumber == geneFromMostFit.innovationNumber)
                .map(geneFromLeastFit => (geneFromMostFit, geneFromLeastFit))
            }
            .flatten
            .map {
              case (left, right) => if (Random.nextDouble() < 0.5) left else right
            }

          val disjointAndExcessGenes = mostFitParent.genome.genes.filterNot(newMatchingGenes.contains(_))

          val inheritedGenes = newMatchingGenes ++ disjointAndExcessGenes

          Genome(inheritedGenes)
        }
      }
    }
  }

  implicit class GenomeSetOps(self: Set[Genome]) {

    def speciate(): Set[Species] = ???
  }

  trait GenomeMutator {
    def mutate(genome: Genome)(implicit settings: MutationSettings): Genome
  }

  final class NEATGenomeMutator() extends GenomeMutator {

    def mutate(genome: Genome)(implicit settings: MutationSettings): Genome = {
      if (settings.addNode()) {
        addNode(genome)
      } else if (settings.addLink()) {
        addLink(genome)
      } else {
        if (settings.mutateWeights()) {
          mutateWeights(genome)
        } else if (settings.toggleEnabled()) {
          toggleEnabled(genome)
        } else {
          genome
        }
      }
    }

    def addNode(genome: Genome): Genome = genome
    def addLink(genome: Genome): Genome = genome
    def mutateWeights(genome: Genome): Genome = genome
    def toggleEnabled(genome: Genome): Genome = genome
  }
}

final case class Species(genomes: Set[Genome]) {
  def evaluate(fitnessFunction: FitnessFunction): List[Organism] = {
    ???
  }

  def epoch(expectedOffspring: Int): Species = {
    ???
  }
}

final case class Population(species: Set[Species]) {

  def epoch(generation: Int, fitnessFunction: FitnessFunction)(implicit
      settings: ReproductionSettings,
      mutator: GenomeMutator): Population = {
    import less.stupid.neat.genotype.GenomeUtils.OrganismListOps
    import less.stupid.neat.genotype.GenomeUtils.GenomeSetOps

    implicit val mutationSettings = settings.mutations

    val organismsBySpecies = species.map(_.evaluate(fitnessFunction).sortBy(_.fitness))
    val overallAverageFitness = organismsBySpecies.map(_.map(_.fitness).sum).sum / organismsBySpecies.map(_.size).sum

    val newSpecies = organismsBySpecies
      .flatMap { organisms =>
        val averageFitness = organisms.map(_.fitness).sum / organisms.size
        val expectedOffspring = (averageFitness / overallAverageFitness).intValue

        (0 until expectedOffspring).map { _ =>
          if (settings.mutateOnly()) {
            organisms.randomOrganism().map(organism => mutator.mutate(organism.genome))
          } else {
            organisms.mate()
          }
        }.flatten
      }
      .speciate()

    Population(newSpecies)
  }
}
object Population {

  def spawn(numberOfInputNodes: Int, numberOfOutputNodes: Int, count: Int = 100): Population = {
    val genomes = Set.fill(count)(Genome.minimal(numberOfInputNodes, numberOfOutputNodes))
    val species = Set(Species(genomes))
    Population(species)
  }
}
