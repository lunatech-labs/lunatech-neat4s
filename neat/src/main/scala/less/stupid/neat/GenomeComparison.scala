package less.stupid.neat

import less.stupid.neat.GenomeComparison.{DisjointGenes, ExcessGenes, MatchingGenes}

import scala.util.Random

final case class GenomeComparison(matching: MatchingGenes, disjoint: DisjointGenes, excess: ExcessGenes) {
  def prettyPrint(): String = {
    s"""GenomeComparison(
       |  ${matching.prettyPrint()}
       |  ${disjoint.prettyPrint()}
       |  EXCESS = (
       |    LEFT  = ${excess.left.map(_.innovationNumber.value)}
       |    RIGHT = ${excess.right.map(_.innovationNumber.value)}
       |  )
       |)
       |""".stripMargin
  }
}
object GenomeComparison {

  type FoldingFunction =
    ((MatchingGenes, DisjointGenes), (Option[Gene], Option[Gene])) => (MatchingGenes, DisjointGenes)

  final case class MatchingGenes(list: List[(Gene, Gene)]) {
    import GenomeComparison.MatchingGenes.matchingOrdering

    val size = list.size

    lazy val weightDifference: BigDecimal =
      list.foldLeft(0.0) { (total, next) =>
        next match {
          case (left, right) => total + Math.abs(left.weight.doubleValue - right.weight.doubleValue)
        }
      }

    def random(): List[Gene] =
      list.map {
        case (left, right) => if (Random.nextBoolean()) left else right
      }

    def +(entry: (Gene, Gene)): MatchingGenes =
      new MatchingGenes((list :+ entry).sorted)

    def prettyPrint(): String =
      s"""MATCHING = ${list.map {
        case (left, right) => s"(${left.innovationNumber.value}, ${right.innovationNumber.value})"
      }}"""
  }
  object MatchingGenes {

    implicit val matchingOrdering: Ordering[(Gene, Gene)] = Ordering.by {
      case (left, _) => left.innovationNumber.value
    }

    val empty: MatchingGenes =
      new MatchingGenes(List.empty)
  }

  final case class DisjointGenes(list: List[(Option[Gene], Option[Gene])]) {
    import GenomeComparison.DisjointGenes.disjointOrdering

    val size = list.size

    lazy val all: List[Gene] =
      list.foldLeft(List.empty[Gene]) { (total, next) =>
        next match {
          case (Some(left), None)  => total :+ left
          case (None, Some(right)) => total :+ right
          case (left, right) =>
            throw new IllegalArgumentException(s"Should have (Some, None) or (None, Some) but received ($left, $right)")
        }
      }

    lazy val right: List[Gene] =
      list.map {
        case (_, right) => right
      }.flatten

    def +(entry: (Option[Gene], Option[Gene])): DisjointGenes =
      new DisjointGenes((list :+ entry).sorted)

    def prettyPrint(): String =
      s"""DISJOINT = ${list.map {
        case (Some(left), None)  => s"(${left.innovationNumber.value}, _)"
        case (None, Some(right)) => s"(_, ${right.innovationNumber.value})"
        case (_, _)              => ""
      }}"""
  }
  object DisjointGenes {

    implicit val disjointOrdering: Ordering[(Option[Gene], Option[Gene])] = Ordering.by {
      case (Some(left), None)  => left.innovationNumber.value
      case (None, Some(right)) => right.innovationNumber.value
      case (left, right) =>
        throw new IllegalArgumentException(s"Should have (Some, None) or (None, Some) but received ($left, $right)")
    }

    val empty: DisjointGenes =
      new DisjointGenes(List.empty)
  }

  final case class ExcessGenes(left: List[Gene], right: List[Gene]) {

    lazy val all: List[Gene] =
      left ++ right

    lazy val size: Int =
      Math.max(left.size, right.size)

    def addLeft(other: Gene): ExcessGenes =
      copy(left = (left :+ other).sortBy(_.innovationNumber.value))

    def addRight(other: Gene): ExcessGenes =
      copy(right = (right :+ other).sortBy(_.innovationNumber.value))
  }
  object ExcessGenes {

    val empty: ExcessGenes =
      ExcessGenes(List.empty, List.empty)
  }

  def fromGenomes(left: Genome, right: Genome): GenomeComparison = {

    val highestInnovationNumberFromLeft = left.genes.map(_.innovationNumber.value).max
    val highestInnovationNumberFromRight = right.genes.map(_.innovationNumber.value).max

    val excessGenes = {
      val leftExcess = left.genes.filter(_.innovationNumber.value > highestInnovationNumberFromRight)
      val rightExcess = right.genes.filter(_.innovationNumber.value > highestInnovationNumberFromLeft)
      ExcessGenes(leftExcess, rightExcess)
    }

    val (matching: MatchingGenes, disjoint: DisjointGenes) = (left.genes ++ right.genes).toSet
      .map((gene: Gene) => gene.innovationNumber)
      .map { innovationNumber =>
        (
          left.genes.find(_.innovationNumber == innovationNumber),
          right.genes.find(_.innovationNumber == innovationNumber))
      }
      .foldLeft((MatchingGenes.empty, DisjointGenes.empty)) { (total, next) =>
        {
          val (matching, disjoint) = total
          next match {
            case (Some(left), Some(right)) => (matching + (left, right), disjoint)
            case (left @ Some(gene), None) if gene.innovationNumber.value < highestInnovationNumberFromRight =>
              (matching, disjoint + (left, None))
            case (None, right @ Some(gene)) if gene.innovationNumber.value < highestInnovationNumberFromLeft =>
              (matching, disjoint + (None, right))
            case (_, _) => total
          }
        }
      }

    new GenomeComparison(matching, disjoint, excessGenes)
  }
}
