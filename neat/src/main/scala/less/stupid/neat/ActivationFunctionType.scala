package less.stupid.neat

import enumeratum.{Enum, EnumEntry}

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
