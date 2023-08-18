package com.lunatech.neat4s.akkastreams

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.{FlowShape, Outlet}
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, Sink, Source}
import com.lunatech.neat4s.NodeType.{Bias, Hidden, Input, Output}
import com.lunatech.neat4s.{
  ActivationFunction,
  ActivationFunctionType,
  Genome,
  MapListElementsToOutputs,
  Network,
  Node,
  NodeId
}
import com.lunatech.neat4s.akkastreams.NetworkNode.{
  HiddenNodeShape,
  InputNodeShape,
  OutputNodeShape,
  OutputValueCollector,
  createHiddenNode,
  createInputNode,
  createOutputNode
}
import com.lunatech.neat4s.akkastreams.NodeInfo.{HiddenNodeInfo, InputNodeInfo, OutputNodeInfo}

import scala.concurrent.Future

final class AkkaStreamsNetworkImpl private (sink: Sink[List[BigDecimal], Future[List[BigDecimal]]])(implicit
    system: ActorSystem[_])
    extends Network {

  override def activate(input: List[BigDecimal]): Future[List[BigDecimal]] =
    Source.single(input).runWith(sink)
}

object AkkaStreamsNetworkImpl {

  sealed trait NodeWrapper
  final case class InputNodeWrapper(node: InputNodeShape) extends NodeWrapper
  final case class HiddenNodeWrapper(node: HiddenNodeShape) extends NodeWrapper
  final case class OutputNodeWrapper(node: OutputNodeShape) extends NodeWrapper

  final case class ConnectionCount(from: Int = 0, to: Int = 0)

  def fromGenome(genome: Genome)(implicit system: ActorSystem[_]): Network = {
    val flow: Flow[List[BigDecimal], List[BigDecimal], NotUsed] = Flow.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      def createNetwork(): (Seq[InputNodeShape], Seq[OutputNodeShape]) = {
        val nodeInfoMap = NodeInfo.fromGenome(genome)

        var inputNodes = Map.empty[NodeId, InputNodeShape]
        var hiddenNodes = Map.empty[NodeId, HiddenNodeShape]
        var outputNodes = Map.empty[NodeId, OutputNodeShape]

        var connectionCounts = Map.empty[NodeId, ConnectionCount]

        def getAndIncrementFrom(nodeId: NodeId): Int = {
          val current = connectionCounts.getOrElse(nodeId, ConnectionCount())
          connectionCounts += (nodeId -> current.copy(from = current.from + 1))
          current.from
        }

        def getAndIncrementTo(nodeId: NodeId): Int = {
          val current = connectionCounts.getOrElse(nodeId, ConnectionCount())
          connectionCounts += (nodeId -> current.copy(to = current.to + 1))
          current.to
        }

        def createNode(nodeInfo: NodeInfo): NodeWrapper = {

          def connect(fromId: NodeId, to: Set[NodeId], out: Int => Outlet[BigDecimal]) = {
            to.foreach { nodeId =>
              nodeInfoMap.get(nodeId).foreach { nodeInfo =>
                val toNode = createNode(nodeInfo)
                val from = getAndIncrementFrom(fromId)
                val to = getAndIncrementTo(nodeId)
                toNode match {
                  case _: InputNodeWrapper =>
                    throw new IllegalArgumentException(
                      "attempt to connect an input node to another input node, this is not allowed")
                  case HiddenNodeWrapper(toNode) =>
                    out(from) ~> toNode.in(to)
                  case OutputNodeWrapper(toNode) =>
                    out(from) ~> toNode.in(to)
                }
              }
            }
          }

          def handleInputNodeInfo(info: InputNodeInfo): NodeWrapper =
            InputNodeWrapper(inputNodes.get(info.id).getOrElse {
              val fromNode = builder.add(
                createInputNode(
                  info.to.size,
                  ActivationFunction.fromActivationFunctionType(info.activationFunctionType)))
              connect(info.id, info.to, fromNode.out)
              inputNodes += (info.id -> fromNode)
              fromNode
            })

          def handleHiddenNodeInfo(info: HiddenNodeInfo): NodeWrapper =
            HiddenNodeWrapper(hiddenNodes.get(info.id).getOrElse {
              val fromNode = builder.add(
                createHiddenNode(
                  info.from.size,
                  info.to.size,
                  ActivationFunction.fromActivationFunctionType(info.activationFunctionType)))
              connect(info.id, info.to, fromNode.out)
              hiddenNodes += (info.id -> fromNode)
              fromNode
            })

          def handleOutputNodeInfo(info: OutputNodeInfo): NodeWrapper =
            OutputNodeWrapper(outputNodes.get(info.id).getOrElse {
              val node = builder.add(
                createOutputNode(
                  info.from.size,
                  ActivationFunction.fromActivationFunctionType(info.activationFunctionType)))
              outputNodes += (info.id -> node)
              node
            })

          nodeInfo match {
            case inputNodeInfo: InputNodeInfo   => handleInputNodeInfo(inputNodeInfo)
            case hiddenNodeInfo: HiddenNodeInfo => handleHiddenNodeInfo(hiddenNodeInfo)
            case outputNodeInfo: OutputNodeInfo => handleOutputNodeInfo(outputNodeInfo)
          }
        }

        nodeInfoMap.values.foreach(createNode)

        (inputNodes.values.toSeq, outputNodes.values.toSeq)
      }

      // Construct the network - input nodes -> hidden nodes -> output nodes
      // we're only interested in the input and output nodes for the rest of this function
      val (inputNodes, outputNodes) = createNetwork()

      // We'll take a `List[BigDecimal]` and send each element to one of the inputs
      val splitInput = builder.add(MapListElementsToOutputs[BigDecimal](inputNodes.size))
      splitInput.out.zipWithIndex.foreach {
        case (out, i) => out ~> inputNodes(i).in
      }

      // We'll take each output node's result and create an ordered list of outputs
      val collector = builder.add(OutputValueCollector[BigDecimal](outputNodes.size))
      outputNodes.zipWithIndex.foreach {
        case (out, i) => out.out ~> collector.in(i)
      }

      // This graph then becomes a `Flow[List[BigDecimal], List[BigDecimal], NotUsed]` as it has a single inlet and a single outlet
      // the list of values to give to the inputs comes in and the list of values from the outputs comes out
      FlowShape(splitInput.in, collector.out)
    })

    // we hook it up to a sink that collects our outputs
    // as soon as we have the first result the stream will stop
    val sink = flow.toMat(Sink.head[List[BigDecimal]])(Keep.right)

    new AkkaStreamsNetworkImpl(sink)
  }
}

private[akkastreams] sealed trait NodeInfo {
  val id: NodeId
  def withTo(nodeInfo: NodeId): NodeInfo
  def withFrom(nodeInfo: NodeId): NodeInfo
}
private[akkastreams] object NodeInfo {

  final case class InputNodeInfo(id: NodeId, activationFunctionType: ActivationFunctionType, to: Set[NodeId])
      extends NodeInfo {
    override def withTo(nodeInfo: NodeId): NodeInfo =
      copy(to = to + nodeInfo)

    override def withFrom(from: NodeId): NodeInfo =
      throw new NotImplementedError("InputNodeInfo.withFrom makes no sense")
  }
  final case class HiddenNodeInfo(
      id: NodeId,
      activationFunctionType: ActivationFunctionType,
      from: Set[NodeId],
      to: Set[NodeId])
      extends NodeInfo {
    override def withTo(nodeInfo: NodeId): NodeInfo =
      copy(to = to + nodeInfo)

    override def withFrom(nodeInfo: NodeId): NodeInfo =
      copy(from = from + nodeInfo)
  }
  final case class OutputNodeInfo(id: NodeId, activationFunctionType: ActivationFunctionType, from: Set[NodeId])
      extends NodeInfo {
    override def withTo(nodeInfo: NodeId): NodeInfo =
      throw new NotImplementedError("OutputNodeInfo.withTo makes no sense")

    override def withFrom(nodeInfo: NodeId): NodeInfo =
      copy(from = from + nodeInfo)
  }

  def fromGenome(genome: Genome): Map[NodeId, NodeInfo] = {

    def createNodeInfo(node: Node): NodeInfo =
      node.nodeType match {
        case Input  => InputNodeInfo(node.nodeId, node.activationFunctionType, Set.empty)
        case Hidden => HiddenNodeInfo(node.nodeId, node.activationFunctionType, Set.empty, Set.empty)
        case Output => OutputNodeInfo(node.nodeId, node.activationFunctionType, Set.empty)
        case Bias   => throw new NotImplementedError("We don't support Bias nodes just yet")
      }

    genome.genes.foldLeft(Map.empty[NodeId, NodeInfo]) { (total, next) =>
      val fromInfo = total.get(next.from.nodeId).getOrElse(createNodeInfo(next.from))
      val toInfo = total.get(next.to.nodeId).getOrElse(createNodeInfo(next.to))

      total + (next.from.nodeId -> fromInfo.withTo(toInfo.id)) + (next.to.nodeId -> toInfo.withFrom(fromInfo.id))
    }
  }
}
