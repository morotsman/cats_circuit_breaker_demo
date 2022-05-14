package com.github.morotsman
package presentation.demo

import presentation.demo.CircuitBreakerState.CircuitBreakerState

import cats.effect.Ref

trait Statistics[F[_]] {
  def requestSent(): F[Unit]

  def requestCompleted(): F[Unit]

  def requestCompletedIn(millis: Long): F[Unit]

  def programCompletedIn(millis: Long): F[Unit]

  def programCalled(): F[Unit]

  def circuitBreakerStateChange(state: CircuitBreakerState): F[Unit]

  def getStatisticsInfo(): F[StatisticsInfo]
}

final case class StatisticsInfo(
                                 pendingRequests: Int,
                                 sentSinceLastReport: Int,
                                 programCalledSinceLastReport: Int,
                                 circuitBreakerState: CircuitBreakerState,
                                 requestsCompletedIn: List[Long],
                                 programCompletedIn: List[Long]
                               )

object StatisticsInfo {
  def make(): StatisticsInfo = StatisticsInfo(
    pendingRequests = 0,
    sentSinceLastReport = 0,
    programCalledSinceLastReport = 0,
    circuitBreakerState = CircuitBreakerState.CLOSED,
    requestsCompletedIn = List.empty,
    programCompletedIn = List.empty
  )
}

object CircuitBreakerState extends Enumeration {
  type CircuitBreakerState = Value
  val OPEN, HALF_OPEN, CLOSED = Value
}

object Statistics {

  def make[F[_]](ref: Ref[F, StatisticsInfo]): Statistics[F] = new Statistics[F] {
    override def requestSent(): F[Unit] =
      ref.modify(s => (s.copy(
        pendingRequests = s.pendingRequests + 1,
        sentSinceLastReport = s.sentSinceLastReport + 1
      ), s))

    override def requestCompleted(): F[Unit] =
      ref.modify(s => (s.copy(pendingRequests = s.pendingRequests - 1), s))

    override def programCalled(): F[Unit] =
      ref.modify(s => (s.copy(programCalledSinceLastReport = s.programCalledSinceLastReport + 1), s))

    override def circuitBreakerStateChange(state: CircuitBreakerState): F[Unit] =
      ref.modify(s => (s.copy(circuitBreakerState = state), s))

    override def getStatisticsInfo(): F[StatisticsInfo] =
      ref.getAndUpdate(s => s.copy(
        sentSinceLastReport = 0,
        programCalledSinceLastReport = 0,
        requestsCompletedIn = List(),
        programCompletedIn = List()
      ))

    override def requestCompletedIn(millis: Long): F[Unit] =
      ref.modify(s => (s.copy(requestsCompletedIn = millis :: s.requestsCompletedIn), s))

    override def programCompletedIn(millis: Long): F[Unit] =
      ref.modify(s => (s.copy(programCompletedIn = millis :: s.programCompletedIn), s))
  }

}
