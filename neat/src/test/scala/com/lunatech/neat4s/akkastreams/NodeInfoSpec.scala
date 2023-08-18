package com.lunatech.neat4s.akkastreams

import com.lunatech.neat4s._
import com.lunatech.neat4s.akkastreams.NodeInfo.{HiddenNodeInfo, InputNodeInfo, OutputNodeInfo}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class NodeInfoSpec extends AnyWordSpecLike with Matchers {

  "fromGenome" should {

    "create a correct map of NodeId -> NodeInfo from which a network can be constructed" in {

      implicit val innovationNumberProvider = new AtomicInnovationNumberProvider
      implicit val genes = new InMemoryGeneRepository
      implicit val nodeIdProvider = new AtomicNodeIdProvider

      val inputNode1 = Node.spawnInputNodeGene()
      val inputNode2 = Node.spawnInputNodeGene()
      val hiddenNode = Node.spawnHiddenNodeGene()
      val outputNode = Node.spawnOutputNodeGene()

      val gene1 = Gene(inputNode1, hiddenNode)
      val gene2 = Gene(inputNode2, outputNode)
      val gene3 = Gene(hiddenNode, outputNode)

      val genome = Genome(List(gene1, gene2, gene3))

      val expectedNodeInfoMap = Map(
        NodeId(0) -> InputNodeInfo(NodeId(0), ActivationFunctionType.None, Set(NodeId(2))),
        NodeId(2) -> HiddenNodeInfo(NodeId(2), ActivationFunctionType.None, Set(NodeId(0)), Set(NodeId(3))),
        NodeId(1) -> InputNodeInfo(NodeId(1), ActivationFunctionType.None, Set(NodeId(3))),
        NodeId(3) -> OutputNodeInfo(NodeId(3), ActivationFunctionType.None, Set(NodeId(1), NodeId(2))))

      val returnedNodeInfoMap = NodeInfo.fromGenome(genome)

      returnedNodeInfoMap shouldBe expectedNodeInfoMap
    }
  }
}
