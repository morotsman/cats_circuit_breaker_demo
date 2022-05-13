package com.github.morotsman
package presentation.slides.demo_slide

import cats.effect.Fiber

final case class CircuitBreakerDemoState[F[_]](
                                                currentAnimation: Option[Fiber[F, Throwable, Unit]],
                                                demoProgram: Option[Fiber[F, Throwable, Unit]],
                                                statisticsPoller: Option[Fiber[F, Throwable, Unit]]
                                              )

object CircuitBreakerDemoState {
  def initial[F[_]](): CircuitBreakerDemoState[F] = CircuitBreakerDemoState[F](
    currentAnimation = None,
    demoProgram = None,
    statisticsPoller = None
  )
}
