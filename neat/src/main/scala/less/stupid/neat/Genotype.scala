package less.stupid.neat

final case class Genotype(nodeGenes: Set[NodeGene], connectionGenes: Set[ConnectionGene]) {

  private lazy val edgeMatrix: Map[(Int, Int), Boolean] = {
//    val map = HashMap<Pair<Int, Int>, Boolean>()
//    for (gene1 in g.nodeGenes) {
//      for (gene2 in g.nodeGenes) {
//        map[Pair(gene1.innov, gene2.innov)] = false
//      }
//    }
//    for (gene in g.connectionGenes) {
//      map.replace(Pair(gene.input, gene.output), true)
//    }
//    return map
    ???
  }

  private lazy val nonEdgeList: List[(Int, Int)] = {
//    //convert the "matrix" to a list
//    val list = ArrayList<Pair<Int, Int>>()
//    for (gene1 in g.nodeGenes) {
//      for (gene2 in g.nodeGenes) {
//        if (!edgeMatrix[Pair(gene1.innov, gene2.innov)]!!) {
//          list.add(Pair(gene1.innov, gene2.innov))
//        }
//      }
//    }
//    return list
    ???
  }

  private def allowedToConnect(a: Int, b: Int): Boolean = {
//    if (a == b) {
//      false
//    } else {
//      //check if a is dependant on b (bfs)
//      val q: Queue < Int > = LinkedList()
//      q.add(a)
//      while (!q.isEmpty()) {
//        val cur = q.poll()
//        if (cur == b) return false
//        g.connectionGenes.filter {
//          connection: ConnectionGene -> connection.output == cur
//        }
//          .forEach {
//            connection: ConnectionGene -> q.add
//            (connection.input)
//          }
//      }
//      true
//    }
    ???
  }

  /**
   * retrieves the list of all edges that are allowed to add (existing and recurrent connections are removed)
   */
  lazy val allowedConnectionList: List[(Int, Int)] =
    nonEdgeList.filter {
      case (a, b) =>
        allowedToConnect(a, b)
    }

  def distanceTo(other: Genotype): BigDecimal = {
//    var w = 0.0
//    var common = 0
//    val map = HashMap<Int, ConnectionGene>()
//    for (i in a.connectionGenes.indices) {
//      map[a.connectionGenes[i].innovation] = a.connectionGenes[i]
//    }
//    for (i in b.connectionGenes.indices) {
//      val mutual = map[b.connectionGenes[i].innovation]
//      mutual?.let {
//        w += abs(it.weight - b.connectionGenes[i].weight)
//        common++
//      }
//    }
//    w /= common.toDouble()
//    if (common == 0) w = 0.0
//    val d = a.connectionGenes.size + b.connectionGenes.size - 2 * common
//    val n = max(1, max(a.connectionGenes.size, b.connectionGenes.size))
//    return COMPAT_1 / n * d + COMPAT_2 * w
    1
  }

//  def mutate()(implicit settings: EvolutionSettings): Genotype = {
//    if (random.nextDouble() < settings.MUTATE_ADD_CONNECTION) mutateAddConnection(child)
//    if (random.nextDouble() < MUTATE_ADD_NODE && child.connectionGenes.isNotEmpty()) mutateSplitConnection(child)
//    if (random.nextDouble() < MUTATE_ENABLE_DISABLE && child.connectionGenes.isNotEmpty()) mutateEnableDisableConnection(
//      child
//    )
//    if (random.nextDouble() < MUTATE_WEIGHT && child.connectionGenes.isNotEmpty()) {
//      if (random.nextDouble() < MUTATE_SINGLE_INSTEAD) {
//        mutateWightSmall(child.connectionGenes[random.nextInt(child.connectionGenes.size)])
//      } else {
//        for (j in child.connectionGenes.indices) {
//          if (random.nextDouble() < MUTATE_WEIGHT_SMALL) {
//            mutateWightSmall(child.connectionGenes[j])
//          } else {
//            mutateWeightRandom(child.connectionGenes[j])
//          }
//        }
//      }
//    }
//    if (random.nextDouble() < MUTATE_FUNCTION) {
//      mutateFunction(child)
//    }
//  }
}

object Genotype {

  private val inputNodeCount: Int = 3
  private val outputNodeCount: Int = 1

  def default(): Genotype = {
    val inputNodes =
      Set.fill(inputNodeCount)(NodeGene(InnovationNumber.next(), NodeType.Input, ActivationFunctionType.None))
    val outputNodes =
      Set.fill(outputNodeCount)(NodeGene(InnovationNumber.next(), NodeType.Output, ActivationFunctionType.None))

    new Genotype(inputNodes ++ outputNodes, Set.empty)
  }
}

final case class EvaluatedGenotype(genotype: Genotype, fitness: Fitness)
