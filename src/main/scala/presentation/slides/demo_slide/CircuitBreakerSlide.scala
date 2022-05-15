package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.{Input, NConsole, Slide}

import cats.implicits._
import cats.MonadError
import cats.effect.{Ref, Temporal}
import presentation.demo.{MayhemState, SourceOfMayhem, Statistics, StatisticsInfo}

final case class CircuitBreakerSlideState[F[_]]
(
  slide: Option[CircuitBreakerDemo[F]]
)

object CircuitBreakerSlideState {
  def make[F[_]](): CircuitBreakerSlideState[F] = CircuitBreakerSlideState[F](
    slide = None
  )
}

final case class CircuitBreakerSlide[F[_] : Temporal : MonadError[*[_], Throwable]]
(
  console: NConsole[F],
  state: Ref[F, CircuitBreakerSlideState[F]]
) extends Slide[F] {

  override def start(): F[Unit] = for {
    sourceOfMayhem <- Ref[F].of(MayhemState.make()).map(SourceOfMayhem.make[F])
    statistics <- Ref[F].of(StatisticsInfo.make()).map(Statistics.make[F])
    circuitBreakerDemoSlide <- Ref[F]
      .of(CircuitBreakerDemoState.make[F]())
      .map(CircuitBreakerDemo[F](
        console = console,
        sourceOfMayhem = sourceOfMayhem,
        statistics = statistics,
        _
      ))
    _ <- state.modify(s => (s.copy(
      slide = Option(circuitBreakerDemoSlide)
    ), s))
    _ <- circuitBreakerDemoSlide.start()
  } yield ()

  override def userInput(input: Input): F[Unit] = for {
    s <- state.get
    _ <- s.slide.traverse(_.userInput(input))
  } yield ()


  override def exit(): F[Unit] = for {
    s <- state.get
    _ <- s.slide.traverse(_.exit())
  } yield ()
}
