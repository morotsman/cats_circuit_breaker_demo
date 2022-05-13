package com.github.morotsman
package presentation

import cats.effect._
import com.github.morotsman.presentation.demo.CircuitBreakerState.CircuitBreakerState
import com.github.morotsman.presentation.demo.{CircuitBreakerState, DemoProgram, MayhemState, SourceOfMayhem, Statistics, StatisticsInfo}
import com.github.morotsman.presentation.slides.demo_slide.{CircuitBreakerDemo, CircuitBreakerDemoState, DemoConfiguration}
import com.github.morotsman.presentation.slides.{Agenda, Start}
import com.github.morotsman.presentation.tools.{Input, NConsole, Presentation, PresentationState}
import io.chrisdavenport.circuit.{Backoff, CircuitBreaker}

import scala.concurrent.duration._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      console <- NConsole.make[IO]()
      (sourceOfMayhem, statistics) <- setupDemoApp()
      circuitBreakerDemoSlide <- Ref[IO].of(CircuitBreakerDemoState.initial[IO]()).map(CircuitBreakerDemo[IO](
        console = console,
        demoProgramFactory = createDemoApp,
        sourceOfMayhem = sourceOfMayhem,
        statistics = statistics,
        _
      ))
      presentation <- Ref[IO]
        .of(PresentationState.initialState(List(
          Start[IO](console),
          Agenda[IO](console),
          circuitBreakerDemoSlide
        )))
        .flatMap(Presentation.make[IO](console, _))
      _ <- presentation.start().start
      _ <- IO(handleInput(console, presentation)).flatten.foreverM
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
      requestTimeout = 5.seconds
    )).map(SourceOfMayhem.make[IO])
    statistics <- Ref[IO].of(StatisticsInfo(
      pendingRequests = 0,
      sentSinceLastReport = 0,
      programCalledSinceLastReport = 0,
      circuitBreakerState = CircuitBreakerState.CLOSED
    )).map(Statistics.make[IO])
  } yield (sourceOfMayhem, statistics)

  def handleInput(c: NConsole[IO], presentation: Presentation[IO]): IO[Input] = for {
    input <- c.read()
    _ <- presentation.userInput(input)
  } yield input

}
