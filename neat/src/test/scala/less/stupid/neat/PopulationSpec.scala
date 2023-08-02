package less.stupid.neat

import less.stupid.neat.TestUtils._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class PopulationSpec extends AnyWordSpecLike with Matchers {

  "spawn" should {

    "create a population of organism with a minimal genome containing correct input/output counts" in {

      implicit val reproductionConfiguration = NoopReproductionConfiguration
      implicit val speciationConfiguration = NoopSpeciationConfiguration
      implicit val nodeIdProvider = new AtomicNodeIdProvider()
      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider()
      implicit val speciesIdProvider: SpeciesIdProvider = new AtomicSpeciesIdProvider()
      implicit val geneRepository = new InMemoryGeneRepository()

      val population = Population.spawn(numberOfInputNodes = 2, numberOfOutputNodes = 1)

      population.species should have size 1
      population.species.toList.head.genomes should have size 100
    }
  }
}
