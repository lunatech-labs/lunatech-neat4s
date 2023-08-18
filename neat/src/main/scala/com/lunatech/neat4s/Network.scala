package com.lunatech.neat4s

import com.lunatech.neat4s.NodeType.{Bias, Hidden, Input, Output}

import scala.concurrent.Future

trait Network {

  def activate(input: List[BigDecimal]): Future[List[BigDecimal]]
}
