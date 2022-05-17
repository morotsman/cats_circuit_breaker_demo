package com.github.morotsman
package presentation.slides.demo_slide

import cats.effect.Fiber
import presentation.demo.DemoProgram
import presentation.tools.Input

final case class DemoConfiguration(
                                    delayBetweenCallToSourceOfMayhemInNanos: Int
                                  )

object DemoConfiguration {
  def make(): DemoConfiguration = DemoConfiguration(
    delayBetweenCallToSourceOfMayhemInNanos = 1000 * 1000 * 1000
  )
}

final case class CircuitBreakerDemoState[F[_]]
(
  demoProgram: Option[DemoProgram[F]],
  demoProgramExecutor: Option[Fiber[F, Throwable, Unit]],
  previousInput: Option[Input],
  demoConfiguration: DemoConfiguration,
  isFailing: Boolean,
  isStarted: Boolean,
  circuitBreakerConfiguration: CircuitBreakerConfiguration
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
  )
}
