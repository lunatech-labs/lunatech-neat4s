package com.lunatech.neat4s.experiments.neat.xor

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.lunatech.neat4s._
import kamon.Kamon

object XOR {

  def main(args: Array[String]): Unit = {
    Kamon.init()

    implicit val innovationNumberProvider = new AtomicInnovationNumberProvider()
    implicit val nodeIdProvider = new AtomicNodeIdProvider()
    implicit val speciesIdProvider = new AtomicSpeciesIdProvider()
    implicit val geneRepository = new InMemoryGeneRepository()

    implicit val system = ActorSystem[Nothing](Behaviors.ignore, "neat-xor")

    val config = NeatConfiguration.of(system)

    implicit val reproductionConfiguration = config.reproduction
    implicit val speciationConfiguration = config.speciation

    val initialPopulation = Population.spawn(numberOfInputNodes = 2, numberOfOutputNodes = 1)
  }
}
