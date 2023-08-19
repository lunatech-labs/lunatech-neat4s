package com.lunatech.neat4s.experiments.neat.xor

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.lunatech.neat4s._
import com.lunatech.neat4s.akkastreams.AkkaStreamsNetworkImpl
import kamon.Kamon
import org.slf4j.LoggerFactory

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object XOR {

  private val log = LoggerFactory.getLogger(getClass)

  final case class InputOutput(inputs: List[BigDecimal], output: BigDecimal)

  def main(args: Array[String]): Unit = {
    Kamon.init()

    implicit val system = ActorSystem[Nothing](Behaviors.ignore, "neat-xor")
    implicit val executionContext = system.executionContext

    val config = NeatConfiguration.of(system)

    implicit val reproductionConfiguration = config.reproduction
    implicit val speciationConfiguration = config.speciation
    implicit val mutationConfiguration = config.mutation
    implicit val innovationNumberProvider = new AtomicInnovationNumberProvider()
    implicit val nodeIdProvider = new AtomicNodeIdProvider()
    implicit val speciesIdProvider = new AtomicSpeciesIdProvider()
    implicit val geneRepository = new InMemoryGeneRepository()
    implicit val genomeMutator = new NEATGenomeMutator()

    val initialPopulation = Population.spawn(numberOfInputNodes = 2, numberOfOutputNodes = 1)

    log.info(s"running experiment for ${config.generations} generations")

    val listOfInputOutputValues = List(
      InputOutput(List(0.0, 0.0), 0.0),
      InputOutput(List(0.0, 1.0), 1.0),
      InputOutput(List(1.0, 0.0), 1.0),
      InputOutput(List(1.0, 1.0), 0.0))

    val fitnessFunction: Genome => Organism =
      genome => {
        val network = AkkaStreamsNetworkImpl.fromGenome(genome)

        val futureFitness = Future
          .sequence(listOfInputOutputValues.map { io =>
            network.activate(io.inputs).map {
              case Nil         => throw new IllegalStateException("No outputs received")
              case head :: Nil => Math.abs((io.output - head).doubleValue)
              case _ :: _      => throw new IllegalStateException("More than one output received")
            }
          })
          .map(errors => Math.pow(4.0 - errors.sum, 2))

        val fitness = Await.result(futureFitness, 3.seconds)

        Organism(genome, fitness)
      }

    (1 to config.generations).foldLeft(initialPopulation) { (currentPopulation, generation) =>
      log.info(s"GENERATION #$generation")
      currentPopulation.epoch(generation, fitnessFunction)
    }
  }
}
