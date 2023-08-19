package com.lunatech.neat4s

import akka.actor.typed.ActorSystem
import pureconfig._
import pureconfig.generic.auto._

import scala.util.Random

final case class ReproductionConfiguration(mutateOnly: BigDecimal) {

  def shouldMutateOnly(): Boolean =
    Random.nextDouble() < mutateOnly
}
final case class SpeciationConfiguration(
    speciesCompatibilityThreshold: BigDecimal,
    disjointCoefficient: BigDecimal,
    excessCoefficient: BigDecimal,
    mutationDifferenceCoefficient: BigDecimal)
final case class MutationConfiguration(
    addNode: BigDecimal,
    addLink: BigDecimal,
    mutateWeights: BigDecimal,
    toggleEnabled: BigDecimal,
    weightMutationPower: BigDecimal) {
  def shouldAddNode(): Boolean =
    Random.nextDouble() < addNode

  def shouldAddLink(): Boolean =
    Random.nextDouble() < addLink

  def shouldMutateWeights(): Boolean =
    Random.nextDouble() < mutateWeights

  def shouldToggleEnabled(): Boolean =
    Random.nextDouble() < toggleEnabled
}
final case class NeatConfiguration(
    generations: Int,
    reproduction: ReproductionConfiguration,
    speciation: SpeciationConfiguration,
    mutation: MutationConfiguration)

object NeatConfiguration {

  def of(system: ActorSystem[_]): NeatConfiguration = {
    val config = system.settings.config
    val source = ConfigSource.fromConfig(config)
    source.at("neat").loadOrThrow[NeatConfiguration]
  }
}
