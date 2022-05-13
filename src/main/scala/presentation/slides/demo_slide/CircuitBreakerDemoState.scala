package com.github.morotsman
package presentation.slides.demo_slide

import cats.effect.Fiber
import com.github.morotsman.presentation.demo.CircuitBreakerState.CircuitBreakerState
import com.github.morotsman.presentation.demo.{CircuitBreakerState, StatisticsInfo}

final case class CircuitBreakerDemoState[F[_]](
                                                currentAnimation: Option[Fiber[F, Throwable, Unit]],
                                                demoProgram: Option[Fiber[F, Throwable, Unit]],
                                                statisticsPoller: Option[Fiber[F, Throwable, Unit]],
                                                statisticsInfo: StatisticsInfo
                                              )

object CircuitBreakerDemoState {
  def initial[F[_]](): CircuitBreakerDemoState[F] = CircuitBreakerDemoState[F](
    currentAnimation = None,
    demoProgram = None,
    statisticsPoller = None,
    statisticsInfo = StatisticsInfo(
      pendingRequests = 0,
      sentSinceLastReport = 0,
      programCalledSinceLastReport = 0,
      circuitBreakerState = CircuitBreakerState.CLOSED
    )
  )
}
