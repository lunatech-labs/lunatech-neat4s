package less.stupid.neat

import enumeratum.{Enum, EnumEntry}

final case class NodeGene(
    innovationNumber: InnovationNumber,
    nodeType: NodeType,
    activationFunctionType: ActivationFunctionType)

final case class InputSignal(value: BigDecimal)
final case class OutputSignal(value: BigDecimal)
final case class Weight(value: BigDecimal)

final case class ConnectionGene(
    inputSignal: InputSignal,
    outputSignal: OutputSignal,
    innovationNumber: InnovationNumber,
    state: ConnectionGeneState,
    weight: Weight)

sealed trait ConnectionGeneState extends EnumEntry
object ConnectionGeneState extends Enum[ConnectionGeneState] {
  override def values: IndexedSeq[ConnectionGeneState] = findValues

  case object Active extends ConnectionGeneState
  case object Inactive extends ConnectionGeneState
}

sealed trait NodeType extends EnumEntry
object NodeType extends Enum[NodeType] {
  override def values: IndexedSeq[NodeType] = findValues

  case object Input extends NodeType
  case object Output extends NodeType
  case object Hidden extends NodeType
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
