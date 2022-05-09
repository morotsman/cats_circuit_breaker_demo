package com.github.morotsman
package presentation.demo

import presentation.demo.CircuitBreakerState.CircuitBreakerState

trait Statistics[F[_]] {
  def requestSent(): F[Unit]

  def requestCompleted(): F[Unit]

  def programCalled(): F[Unit]

  def circuitBreakerStateChange(state: CircuitBreakerState): F[Unit]

  def getStatisticsInfo(): F[StatisticsInfo]
}

final case class StatisticsInfo()

object CircuitBreakerState extends Enumeration {
  type CircuitBreakerState = Value
  val OPEN, HALF_OPEN, CLOSED = Value
}
