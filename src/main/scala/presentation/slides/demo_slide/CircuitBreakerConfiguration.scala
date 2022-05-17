package com.github.morotsman
package presentation.slides.demo_slide

import scala.concurrent.duration.{DurationInt, FiniteDuration}

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
