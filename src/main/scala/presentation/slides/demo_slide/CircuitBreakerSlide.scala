package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.{Input, NConsole, Slide}

import cats.implicits._
import cats.effect.implicits._
import cats.effect.{Ref, Temporal}
import presentation.demo.{MayhemState, SourceOfMayhem, Statistics, StatisticsState}
import presentation.slides.demo_slide.animations.{Animator, AnimatorState}

final case class CircuitBreakerSlideState[F[_]]
(
  controlPanel: Option[ControlPanel[F]],
)

object CircuitBreakerSlideState {
  def make[F[_]](): CircuitBreakerSlideState[F] = CircuitBreakerSlideState[F](
    controlPanel = None,
  )
}

final case class CircuitBreakerSlide[F[_] : Temporal]
(
  console: NConsole[F],
  state: Ref[F, CircuitBreakerSlideState[F]]
) extends Slide[F] {

  override def show(): F[Unit] = for {
    sourceOfMayhem <- Ref[F].of(MayhemState.make()).map(SourceOfMayhem.make[F])
    statistics <- Ref[F].of(StatisticsState.make()).map(Statistics.make[F])
    demoProgramExecutor <- Ref[F].of(DemoProgramExecutorState.make[F]()).flatMap(state => DemoProgramExecutor.make(
      state = state,
      sourceOfMayhem = sourceOfMayhem,
      statistics = statistics
    ))
    controlPanel <- Ref[F].of(ControlPanelState.make[F]()).map(state => ControlPanel.make[F](
      state = state,
      sourceOfMayhem = sourceOfMayhem,
      demoProgramExecutor = demoProgramExecutor
    ))
    animator <- Ref[F].of(AnimatorState.make()).flatMap(state =>
      Animator.make[F](state, controlPanel, statistics, sourceOfMayhem, demoProgramExecutor, console)
    )
    _ <- state.modify(s => (s.copy(
      controlPanel = Option(controlPanel)
    ), s))
    _ <- (
      statistics.aggregate(),
      demoProgramExecutor.execute(),
      animator.animate()
    ).parTupled.background.use { _ =>
      demoProgramExecutor.execute()
    }
  } yield ()

  override def userInput(input: Input): F[Unit] = for {
    s <- state.get
    _ <- s.controlPanel.traverse(_.userInput(input))
  } yield ()

}
