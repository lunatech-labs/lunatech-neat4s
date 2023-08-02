package less.stupid.neat

import java.util.concurrent.atomic.AtomicInteger

final case class InnovationNumber(value: Int)

object InnovationNumber {

  private val count = new AtomicInteger(0)

  def next(): InnovationNumber =
    InnovationNumber(count.getAndIncrement())
}
