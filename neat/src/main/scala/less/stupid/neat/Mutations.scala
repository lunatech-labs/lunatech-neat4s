package less.stupid.neat

import scala.util.Random

class Mutation[+A](value: => A) {
  private lazy val internal: A = value
  def flatMap[B](f: (=> A) => Mutation[B]): Mutation[B] = f(internal)
  def map[B](f: A => B): Mutation[B] = flatMap(x => Mutation(f(x)))
  def result: A = internal

  def ~>[B](f: A => Mutation[B]): Mutation[B] = f(internal)
}
object Mutation {
  def apply[A](value: => A): Mutation[A] = new Mutation(value)
}

object Mutations {

  implicit class GenotypeOps(self: Genotype) {

    private val random: Random = new Random()

    def mutate()(implicit settings: EvolutionSettings): Genotype =
      (Mutation(self) ~>
      maybeAddConnection ~>
      maybeSplitConnection ~>
      maybeToggleConnectionEnabled ~>
      maybeChangeActivationFunction).result

    private def maybeAddConnection(genotype: Genotype)(implicit settings: EvolutionSettings): Mutation[Genotype] =
      cond[Genotype](() => random.nextDouble() > settings.MUTATE_ADD_CONNECTION)(identity)(genotype)

    private def maybeSplitConnection(genotype: Genotype)(implicit settings: EvolutionSettings): Mutation[Genotype] =
      cond[Genotype](() => random.nextDouble() > settings.MUTATE_ADD_NODE)(identity)(genotype)

    private def maybeToggleConnectionEnabled(genotype: Genotype)(implicit
        settings: EvolutionSettings): Mutation[Genotype] =
      cond[Genotype](() => random.nextDouble() > settings.MUTATE_ENABLE_DISABLE)(identity)(genotype)

    private def maybeChangeActivationFunction(genotype: Genotype)(implicit
        settings: EvolutionSettings): Mutation[Genotype] =
      cond[Genotype](() => random.nextDouble() > settings.MUTATE_FUNCTION)(identity)(genotype)

    private def cond[A](predicate: () => Boolean)(f: A => A): A => Mutation[A] =
      genotype => Mutation(if (predicate()) f(genotype) else genotype)
  }
}
