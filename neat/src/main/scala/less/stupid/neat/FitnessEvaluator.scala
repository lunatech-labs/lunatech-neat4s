package less.stupid.neat

trait FitnessEvaluator {

  def evaluate(genotype: Genotype): Fitness
}
