package less.stupid.neat.genotype

import scala.util.Random

final case class ReproductionConfiguration(mutateOnly: BigDecimal, mutations: MutationConfiguration)
final case class MutationConfiguration(
    addNode: BigDecimal,
    addLink: BigDecimal,
    mutateWeights: BigDecimal,
    toggleEnabled: BigDecimal)
final case class NeatConfiguration(reproduction: ReproductionConfiguration)

trait ReproductionSettings {
  val mutations: MutationSettings
  def mutateOnly(): Boolean
}

class ReproductionSettingsImpl(configuration: ReproductionConfiguration) extends ReproductionSettings {

  val mutations: MutationSettings =
    new MutationSettingsImpl(configuration.mutations)

  def mutateOnly(): Boolean =
    Random.nextDouble() < configuration.mutateOnly
}

trait MutationSettings {
  def addNode(): Boolean
  def addLink(): Boolean
  def mutateWeights(): Boolean
  def toggleEnabled(): Boolean
}

class MutationSettingsImpl(configuration: MutationConfiguration) extends MutationSettings {

  def addNode(): Boolean =
    Random.nextDouble() < configuration.addNode

  def addLink(): Boolean =
    Random.nextDouble() < configuration.addLink

  def mutateWeights(): Boolean =
    Random.nextDouble() < configuration.mutateWeights

  def toggleEnabled(): Boolean =
    Random.nextDouble() < configuration.toggleEnabled
}
