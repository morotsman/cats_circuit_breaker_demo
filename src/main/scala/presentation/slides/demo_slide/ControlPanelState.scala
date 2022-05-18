package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.Input

final case class ControlPanelState[F[_]]
(
  previousInput: Option[Input],
)

object ControlPanelState {
  def make[F[_]](): ControlPanelState[F] = ControlPanelState[F](
    previousInput = None,
  )
}
