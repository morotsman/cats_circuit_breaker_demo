package com.github.morotsman
package presentation.slides.demo_slide

import cats.effect.Fiber
import presentation.demo.{DemoProgram, StatisticsInfo}
import presentation.tools.Input

import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class CircuitBreakerConfiguration(
                                              maxFailures: Int,
                                              resetTimeout: FiniteDuration,
                                              maxResetTimeout: FiniteDuration
                                            )

object CircuitBreakerConfiguration {
  def make(): CircuitBreakerConfiguration = CircuitBreakerConfiguration(
    maxFailures = 5,
    resetTimeout = 3.seconds,
    maxResetTimeout = 30.seconds
  )
}

final case class CircuitBreakerDemoState[F[_]](
                                                currentAnimation: Option[Fiber[F, Throwable, Unit]],
                                                demoProgram: Option[DemoProgram[F]],
                                                demoProgramExecutor: Option[Fiber[F, Throwable, Unit]],
                                                statisticsPoller: Option[Fiber[F, Throwable, Unit]],
                                                statisticsInfo: StatisticsInfo,
                                                previousInput: Option[Input],
                                                demoConfiguration: DemoConfiguration,
                                                isFailing: Boolean,
                                                isStarted: Boolean,
                                                circuitBreakerConfiguration: CircuitBreakerConfiguration
                                              )

object CircuitBreakerDemoState {
  def initial[F[_]](): CircuitBreakerDemoState[F] = CircuitBreakerDemoState[F](
    currentAnimation = None,
    demoProgram = None,
    demoProgramExecutor = None,
    statisticsPoller = None,
    statisticsInfo = StatisticsInfo.make(),
    previousInput = None,
    demoConfiguration = DemoConfiguration.make(),
    isFailing = false,
    isStarted = false,
    circuitBreakerConfiguration = CircuitBreakerConfiguration.make()
  )
}
