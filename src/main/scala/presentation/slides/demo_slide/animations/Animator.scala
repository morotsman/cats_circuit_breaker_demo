package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.implicits._
import cats.Monad
import cats.effect.{Ref, Temporal}
import presentation.demo.{CircuitBreakerState, SourceOfMayhem, Statistics}
import presentation.slides.demo_slide.{ControlPanel, DemoProgramExecutor}
import presentation.tools.NConsole
import presentation.slides.demo_slide.animations.AnimationState.{AnimationMapper, AnimationState, CLOSED_FAILING, CLOSED_SUCCEED, NOT_STARTED}
import presentation.slides.demo_slide.animations.ClosedFailure.ClosedFailureAnimation
import presentation.slides.demo_slide.animations.ClosedSuccess.ClosedSuccessAnimation
import presentation.slides.demo_slide.animations.Static.staticAnimation

import scala.concurrent.duration.DurationInt

object AnimationState extends Enumeration {
  type AnimationState = Value
  val NOT_STARTED, CLOSED_SUCCEED, CLOSED_FAILING = Value

  val AnimationMapper = Map(
    NOT_STARTED -> staticAnimation,
    CLOSED_SUCCEED -> ClosedSuccessAnimation,
    CLOSED_FAILING -> ClosedFailureAnimation
  )
}

final case class AnimatorState
(
  animationState: AnimationState
)

object AnimatorState {
  def make(): AnimatorState = AnimatorState(NOT_STARTED)
}

trait Animator[F[_]] {
  def animate(): F[Unit]
}

object Animator {

  def make[F[_] : Temporal]
  (
    state: Ref[F, AnimatorState],
    controlPanel: ControlPanel[F],
    statistics: Statistics[F],
    sourceOfMayhem: SourceOfMayhem[F],
    demoProgramExecutor: DemoProgramExecutor[F],
    console: NConsole[F]
  ): F[Animator[F]] = Monad[F].pure(new Animator[F] {
    override def animate(): F[Unit] = {
      def animate(frame: Int): F[Unit] = for {
        animatorState <- state.get
        controlPanelState <- controlPanel.getState()
        demoProgramExecutorState <- demoProgramExecutor.getState()
        statisticsInfo <- statistics.getStatisticsInfo()
        mayhemState <- sourceOfMayhem.mayhemState()
        animationState = if (
          controlPanelState.isStarted && statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && !controlPanelState.isFailing
        ) {
          CLOSED_SUCCEED
        } else if (
          controlPanelState.isStarted && statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && controlPanelState.isFailing
        ) {
          CLOSED_FAILING
        } else {
          NOT_STARTED
        }
        updated <- if (animatorState.animationState != animationState) {
          state.modify(s => (s.copy(
            animationState = animationState
          ), s)).map(_ => true)
        } else {
          Monad[F].pure(false)
        }
        frameToShow = if (updated) 0 else frame
        animation = AnimationMapper(animationState)
        _ <- console.writeString(
          animation(frameToShow)(
            statisticsInfo,
            controlPanelState.previousInput,
            controlPanelState.isStarted,
            mayhemState,
            demoProgramExecutorState.circuitBreakerConfiguration
          )) >>
          Temporal[F].sleep(500.milli) >>
          console.clear() >>
          animate(if (frameToShow < animation.size - 1) frameToShow + 1 else 0)
      } yield ()

      animate(0)
    }
  })
}





