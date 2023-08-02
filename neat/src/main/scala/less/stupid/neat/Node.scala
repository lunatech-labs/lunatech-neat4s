package less.stupid.neat

import enumeratum.{Enum, EnumEntry}

import java.util.concurrent.atomic.AtomicInteger

final case class NodeId(value: Int)

trait NodeIdProvider {
  def next(): NodeId
}

final class AtomicNodeIdProvider extends NodeIdProvider {
  private val count = new AtomicInteger()

  def next(): NodeId =
    NodeId(count.getAndIncrement())
}

sealed trait NodeType extends EnumEntry
object NodeType extends Enum[NodeType] {
  override def values: IndexedSeq[NodeType] = findValues

  object Input extends NodeType
  object Bias extends NodeType
  object Hidden extends NodeType
  object Output extends NodeType
}

final case class Node(nodeId: NodeId, nodeType: NodeType, activationFunctionType: ActivationFunctionType)
object Node {

  def spawnInputNodeGene()(implicit nodeIdProvider: NodeIdProvider): Node = {
    Node(nodeIdProvider.next(), NodeType.Input, ActivationFunctionType.None)
  }

//  def spawnBiasNodeGene()(implicit nodeIdProvider: NodeIdProvider): Node = {
//    BiasNodeGene(nodeIdProvider.next(), ActivationFunctionType.None)
//  }

  def spawnHiddenNodeGene()(implicit nodeIdProvider: NodeIdProvider): Node = {
    Node(nodeIdProvider.next(), NodeType.Hidden, ActivationFunctionType.None)
  }

  def spawnOutputNodeGene()(implicit nodeIdProvider: NodeIdProvider): Node = {
    Node(nodeIdProvider.next(), NodeType.Output, ActivationFunctionType.None)
  }
}
