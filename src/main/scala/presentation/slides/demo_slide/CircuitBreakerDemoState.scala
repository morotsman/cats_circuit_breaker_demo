package com.github.morotsman
package presentation.slides.demo_slide

import cats.effect.Fiber
import presentation.demo.DemoProgram
import presentation.tools.Input
import presentation.slides.demo_slide.animations.Animation.AnimationState.{AnimationState, NOT_STARTED}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class DemoConfiguration(
                                    delayBetweenCallToSourceOfMayhemInNanos: Int
                                  )

object DemoConfiguration {
  def make(): DemoConfiguration = DemoConfiguration(
    delayBetweenCallToSourceOfMayhemInNanos = 1000 * 1000 * 1000
  )
}

final case class CircuitBreakerConfiguration(
                                              maxFailures: Int,
                                              resetTimeout: FiniteDuration,
                                              maxResetTimeout: FiniteDuration
                                            )

object CircuitBreakerConfiguration {
  def make(): CircuitBreakerConfiguration = CircuitBreakerConfiguration(
    maxFailures = 4,
    resetTimeout = 3.seconds,
    maxResetTimeout = 30.seconds
  )
}

final case class CircuitBreakerDemoState[F[_]](
                                                demoProgram: Option[DemoProgram[F]],
                                                demoProgramExecutor: Option[Fiber[F, Throwable, Unit]],
                                                previousInput: Option[Input],
                                                demoConfiguration: DemoConfiguration,
                                                isFailing: Boolean,
                                                isStarted: Boolean,
                                                circuitBreakerConfiguration: CircuitBreakerConfiguration,
                                                animationState: AnimationState
                                              )

object CircuitBreakerDemoState {
  def make[F[_]](): CircuitBreakerDemoState[F] = CircuitBreakerDemoState[F](
    demoProgram = None,
    demoProgramExecutor = None,
    previousInput = None,
    demoConfiguration = DemoConfiguration.make(),
    isFailing = false,
    isStarted = false,
    circuitBreakerConfiguration = CircuitBreakerConfiguration.make(),
    animationState = NOT_STARTED
  )
}
