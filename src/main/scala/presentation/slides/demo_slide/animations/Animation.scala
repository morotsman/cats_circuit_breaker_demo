package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.slides.demo_slide.animations.Animation.AnimationState.{AnimationState, CLOSED_FAILING, CLOSED_SUCCEED, NOT_STARTED}
import presentation.slides.demo_slide.animations.Static.staticAnimation
import presentation.slides.demo_slide.animations.ClosedFailure.ClosedFailureAnimation
import presentation.slides.demo_slide.animations.ClosedSuccess.ClosedSuccessAnimation

object Animation {

  object AnimationState extends Enumeration {
    type AnimationState = Value
    val NOT_STARTED, CLOSED_SUCCEED, CLOSED_FAILING  = Value
  }

  val AnimationMapper = Map(
    NOT_STARTED -> staticAnimation,
    CLOSED_SUCCEED -> ClosedSuccessAnimation,
    CLOSED_FAILING -> ClosedFailureAnimation
  )



}
