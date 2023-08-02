package less.stupid.neat.phenotype

import less.stupid.neat.genotype.{Gene, Genome}

sealed trait NetworkNode
object NetworkNode {
  def fromGene(gene: Gene): NetworkNode = {
    ???
  }
}

sealed trait IncomingNode
sealed trait OutgoingNode

case class InputNode(outgoing: Set[OutgoingNode]) extends NetworkNode with IncomingNode
case class BiasNode(outgoing: Set[OutgoingNode]) extends NetworkNode with IncomingNode
case class OutputNode(incoming: Set[IncomingNode]) extends NetworkNode with OutgoingNode
case class HiddenNode(incoming: Set[IncomingNode], outgoing: Set[OutgoingNode])
    extends NetworkNode
    with IncomingNode
    with OutgoingNode

final case class Network(inputs: Set[IncomingNode], hidden: Set[HiddenNode], outputs: Set[OutputNode]) {

  def activate(values: Set[BigDecimal]): Set[BigDecimal] = {
    ???
  }
}

object Network {

  def fromGenome(genome: Genome): Network = {
    ???
  }
}
