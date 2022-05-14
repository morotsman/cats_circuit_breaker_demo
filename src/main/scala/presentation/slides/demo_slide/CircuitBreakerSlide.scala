package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.{Input, NConsole, Slide}

import cats.implicits._
import cats.MonadError
import cats.effect.{Ref, Temporal}
import com.github.morotsman.presentation.demo.{CircuitBreakerState, DemoProgram, MayhemState, SourceOfMayhem, Statistics, StatisticsInfo}
import io.chrisdavenport.circuit.{Backoff, CircuitBreaker}

import scala.concurrent.duration.DurationInt

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
    sourceOfMayhem <- Ref[F].of(MayhemState(
      isFailing = false,
      successLatency = 30.millis,
      requestTimeout = 5.seconds
    )).map(SourceOfMayhem.make[F])
    statistics <- Ref[F].of(StatisticsInfo.make()).map(Statistics.make[F])
    configuration = CircuitBreakerConfiguration.make()
    demoProgram <- CircuitBreaker.of[F](
      maxFailures = configuration.maxFailures,
      resetTimeout = configuration.resetTimeout,
      backoff = Backoff.exponential,
      maxResetTimeout = configuration.maxResetTimeout,
      onOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.OPEN),
      onClosed = statistics.circuitBreakerStateChange(CircuitBreakerState.CLOSED),
      onRejected = MonadError[F, Throwable].unit,
      onHalfOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.HALF_OPEN)
    ).map { circuitBreaker =>
      DemoProgram[F](
        sourceOfMayhem = sourceOfMayhem,
        circuitBreaker = circuitBreaker,
        statistics = statistics
      )
    }
    circuitBreakerDemoSlide <- Ref[F]
      .of(CircuitBreakerDemoState.initial[F]())
      .map(CircuitBreakerDemo[F](
        console = console,
        demoProgram = demoProgram,
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
