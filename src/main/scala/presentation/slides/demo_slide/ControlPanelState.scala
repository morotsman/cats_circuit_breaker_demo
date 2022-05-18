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

final case class ControlPanelState[F[_]]
(
  previousInput: Option[Input],
  isFailing: Boolean,
  isStarted: Boolean,
)

object ControlPanelState {
  def make[F[_]](): ControlPanelState[F] = ControlPanelState[F](
    previousInput = None,
    isFailing = false,
    isStarted = false,
  )
}
