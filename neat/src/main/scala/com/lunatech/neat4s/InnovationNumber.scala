package com.lunatech.neat4s

import java.util.concurrent.atomic.AtomicInteger

final case class InnovationNumber(value: Int)

trait InnovationNumberProvider {
  def next(): InnovationNumber
}

final class AtomicInnovationNumberProvider extends InnovationNumberProvider {

  private val count = new AtomicInteger(0)

  def next(): InnovationNumber =
    InnovationNumber(count.getAndIncrement())
}
