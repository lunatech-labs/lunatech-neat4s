package com.lunatech.neat4s

sealed trait ActivationFunction extends Function[BigDecimal, BigDecimal]
object ActivationFunction {
  val noop: ActivationFunction = new ActivationFunction {
    override def apply(input: BigDecimal): BigDecimal = input
  }

  def fromActivationFunctionType(activationFunctionType: ActivationFunctionType): ActivationFunction =
    noop
}
