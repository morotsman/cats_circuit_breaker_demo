package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.implicits._
import cats.{FlatMap, Monad}
import cats.effect.{Ref, Temporal}
import presentation.demo.{CircuitBreakerState, SourceOfMayhem}
import presentation.slides.demo_slide.CircuitBreakerDemoState

import com.github.morotsman.presentation.slides.demo_slide.animations.Animation.AnimationMapper
import com.github.morotsman.presentation.slides.demo_slide.animations.Animation.AnimationState.{CLOSED_FAILING, CLOSED_SUCCEED, NOT_STARTED}
import com.github.morotsman.presentation.tools.NConsole

import scala.concurrent.duration.DurationInt

trait Animator[F[_]] {
  def animate(): F[Unit]
}

object Animator {

  def make[F[_] : Monad : Temporal]
  (
    state: Ref[F, CircuitBreakerDemoState[F]],
    sourceOfMayhem: SourceOfMayhem[F],
    console: NConsole[F]
  ): F[Animator[F]] = Monad[F].pure(new Animator[F] {
    override def animate(): F[Unit] = {
      def animate(frame: Int): F[Unit] = {
        for {
          s <- state.get
          mayhemState <- sourceOfMayhem.mayhemState()
          animationState = if (
            s.isStarted && s.statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && !s.isFailing
          ) {
            CLOSED_SUCCEED
          } else if (
            s.isStarted && s.statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && s.isFailing
          ) {
            CLOSED_FAILING
          } else {
            NOT_STARTED
          }
          updated <- if (s.animationState != animationState) {
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
              s.statisticsInfo,
              s.previousInput,
              s.isStarted,
              mayhemState,
              s.circuitBreakerConfiguration
            )) >>
            Temporal[F].sleep(500.milli) >>
            console.clear() >>
            animate(if (frameToShow < animation.size - 1) frameToShow + 1 else 0)
        }

        yield ()
      }


      animate(0)
    }
  })
}





