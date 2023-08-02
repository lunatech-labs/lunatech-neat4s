package less.stupid.neat

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.Map

trait GeneRepository {

  def getOrLoad(from: Node, to: Node, loadValue: (Node, Node) => Gene): Gene
}

final class InMemoryGeneRepository()(implicit innovationNumberProvider: InnovationNumberProvider)
    extends GeneRepository {

  val genes: Map[(Node, Node), Gene] = new TrieMap[(Node, Node), Gene]()

  override def getOrLoad(from: Node, to: Node, loadValue: (Node, Node) => Gene): Gene = {
    val key = (from, to)
    genes.get(key).getOrElse {
      val value = loadValue(from, to)
      genes.put(key, value)
      value
    }
  }
}

// TODO: Look at restricting direct instantiation of a Gene from the case class constructor.
// Currently, users should never NEED to create Genes directly, but would be nice to restrict this
// making the case class `sealed abstract` prevents us from testing in GenomeComparisonSpec so it's not obvious _how_ to do it
final case class Gene(
    innovationNumber: InnovationNumber,
    from: Node,
    to: Node,
    weight: BigDecimal,
    enabled: Boolean = true)
object Gene {
  def apply(from: Node, to: Node)(implicit
      innovationNumberProvider: InnovationNumberProvider,
      repository: GeneRepository): Gene = {
    repository.getOrLoad(from, to, (from, to) => new Gene(innovationNumberProvider.next(), from, to, BigDecimal(1)))
  }
}
