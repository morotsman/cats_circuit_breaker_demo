package com.github.morotsman
package presentation

import cats.effect._
import presentation.slides.demo_slide.{CircuitBreakerSlide, CircuitBreakerSlideState}
import presentation.slides.{Agenda, Start}
import presentation.tools.{Input, NConsole, Presentation, PresentationState}

import cats.implicits.catsSyntaxTuple2Parallel

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      console <- NConsole.make[IO]()
      circuitBreakerSlide <- Ref[IO].of(CircuitBreakerSlideState.make[IO]()).map(CircuitBreakerSlide[IO](console, _))
      presentation <- Ref[IO]
        .of(PresentationState.initialState(List(
          Start[IO](console),
          Agenda[IO](console),
          circuitBreakerSlide
        )))
        .flatMap(Presentation.make[IO](console, _))
      _ <- (
        presentation.start(),
        IO(handleInput(console, presentation)).flatten.foreverM
        ).parTupled
    } yield ()).map(_ => ExitCode.Success)
  }

  def handleInput(c: NConsole[IO], presentation: Presentation[IO]): IO[Input] = for {
    input <- c.read()
    _ <- presentation.userInput(input)
  } yield input

}
