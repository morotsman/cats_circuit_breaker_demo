package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.{Input, NConsole, Slide}

import cats.implicits._
import cats.effect.implicits._
import cats.{Monad, MonadError}
import cats.effect.{Fiber, Ref, Spawn, Temporal}
import presentation.demo.{MayhemState, SourceOfMayhem, Statistics, StatisticsInfo, StatisticsState}

import com.github.morotsman.presentation.slides.demo_slide.animations.{Animator, AnimatorState}

final case class CircuitBreakerSlideState[F[_]]
(
  slide: Option[CircuitBreakerDemo[F]],
  animator: Option[Fiber[F, Throwable, Unit]],
  statisticsAggregator: Option[Fiber[F, Throwable, Unit]],
)

object CircuitBreakerSlideState {
  def make[F[_]](): CircuitBreakerSlideState[F] = CircuitBreakerSlideState[F](
    slide = None,
    animator = None,
    statisticsAggregator = None
  )
}

final case class CircuitBreakerSlide[F[_] : Monad : Temporal : Spawn]
(
  console: NConsole[F],
  state: Ref[F, CircuitBreakerSlideState[F]]
) extends Slide[F] {

  override def show(): F[Unit] = for {
    sourceOfMayhem <- Ref[F].of(MayhemState.make()).map(SourceOfMayhem.make[F])
    statistics <- Ref[F].of(StatisticsState.make()).map(Statistics.make[F])
    circuitBreakerDemoSlide <- Ref[F].of(CircuitBreakerDemoState.make[F]()).map(state => CircuitBreakerDemo[F](
      console = console,
      sourceOfMayhem = sourceOfMayhem,
      statistics = statistics,
      state = state
    ))
    animator <- Ref[F].of(AnimatorState.make()).flatMap(state =>
      Animator.make[F] (circuitBreakerDemoSlide, statistics, sourceOfMayhem, console, state)
    )
    _ <- state.modify(s => (s.copy(
      slide = Option(circuitBreakerDemoSlide)
    ), s))
    animator <- animator.animate().start
    statisticsAggregator <- statistics.aggregate().start
    _ <- circuitBreakerDemoSlide.show()
    _ <- state.modify(s => (s.copy(
      slide = Option(circuitBreakerDemoSlide),
      animator = Option(animator),
      statisticsAggregator = Option(statisticsAggregator)
    ), s))
  } yield ()

  override def userInput(input: Input): F[Unit] = for {
    s <- state.get
    _ <- s.slide.traverse(_.userInput(input))
  } yield ()


  override def exit(): F[Unit] = for {
    s <- state.get
    _ <- s.slide.traverse(_.exit())
    _ <- s.animator.traverse(_.cancel)
    _ <- s.statisticsAggregator.traverse(_.cancel)
  } yield ()
}
