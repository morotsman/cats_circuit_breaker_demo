package com.github.morotsman
package presentation

import cats.effect._
import com.github.morotsman.presentation.demo.CircuitBreakerState.CircuitBreakerState
import com.github.morotsman.presentation.demo.{CircuitBreakerState, DemoProgram, MayhemState, SourceOfMayhem, Statistics, StatisticsInfo}
import io.chrisdavenport.circuit.{Backoff, CircuitBreaker}

import scala.concurrent.duration._

object CircuitBreakerPresentation extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      console <- NConsole.make[IO]()
      (sourceOfMayhem, statistics) <- setupDemoApp()
      presentation <- Ref[IO]
        .of(PresentationState(0, List(
          Start[IO](console),
          Agenda[IO](console),
          StaticViewCircuitBreaker[IO](console),
          CircuitBreakerDemo[IO](
            console = console,
            demoProgramFactory = createDemoApp,
            sourceOfMayhem = sourceOfMayhem,
            statistics = statistics
          )
        )))
        .flatMap(Presentation.make[IO](console, _))
      firstSlide <- presentation.start().start
      ref <- Ref[IO].of(CurrentSlide(firstSlide))
      _ <- IO(handleInput(console, presentation, ref)).flatten.foreverM
    } yield ()).map(_ => ExitCode.Success)
  }

  private def createDemoApp(
                             configuration: DemoConfiguration,
                             sourceOfMayhem: SourceOfMayhem[IO],
                             statistics: Statistics[IO]
                           ): IO[DemoProgram[IO]] = {
    CircuitBreaker.of[IO](
      maxFailures = configuration.maxFailures,
      resetTimeout = configuration.resetTimeout,
      backoff = Backoff.exponential,
      maxResetTimeout = configuration.maxResetTimeout,
      onOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.OPEN),
      onClosed = statistics.circuitBreakerStateChange(CircuitBreakerState.CLOSED),
      onRejected = IO.unit,
      onHalfOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.HALF_OPEN)
    ).map { circuitBreaker =>
      DemoProgram[IO](
        sourceOfMayhem = sourceOfMayhem,
        circuitBreaker = circuitBreaker,
        statistics = statistics
      )
    }
  }

  private def setupDemoApp() = for {
    sourceOfMayhem <- Ref[IO].of(MayhemState(
      isFailing = false,
      successLatency = 30.millis,
      requestTimeout = 1.seconds
    )).map(SourceOfMayhem.make[IO])
    statistics <- Ref[IO].of(StatisticsInfo(
      pendingRequests = 0,
      sentSinceLastReport = 0,
      programCalledSinceLastReport = 0,
      circuitBreakerState = CircuitBreakerState.CLOSED
    )).map(Statistics.make[IO])
  } yield (sourceOfMayhem, statistics)

  case class CurrentSlide(fiber: FiberIO[Unit])

  def handleInput(c: NConsole[IO], presentation: Presentation[IO], ref: Ref[IO, CurrentSlide]): IO[Input] = for {
    input <- c.read()
    state <- ref.get
    _ <- input match {
      case Key(k) if k == SpecialKey.Left =>
        for {
          f <- state.fiber.cancel >> presentation.previousSlide().start
          _ <- ref.set(CurrentSlide(f))
        } yield ()
      case Key(k) if k == SpecialKey.Right =>
        for {
          f <- state.fiber.cancel >> presentation.nextSlide().start
          _ <- ref.set(CurrentSlide(f))
        } yield ()
      case Key(k) if k == SpecialKey.Esc =>
        for {
          f <- state.fiber.cancel >> presentation.exit().start
          _ <- ref.set(CurrentSlide(f))
        } yield ()
      case _ => for {
        f <- state.fiber.cancel >> presentation.userInput(input).start
        _ <- ref.set(CurrentSlide(f))
      } yield ()
    }
  } yield input

}
