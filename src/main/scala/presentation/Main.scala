package com.github.morotsman
package presentation

import cats.effect._
import com.github.morotsman.presentation.slides.demo_slide.{CircuitBreakerSlide, CircuitBreakerSlideState}
import com.github.morotsman.presentation.slides.{Agenda, Start}
import com.github.morotsman.presentation.tools.{Input, NConsole, Presentation, PresentationState}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      console <- NConsole.make[IO]()
      slide <- Ref[IO].of(CircuitBreakerSlideState.make[IO]()).map(CircuitBreakerSlide[IO](console, _))
      presentation <- Ref[IO]
        .of(PresentationState.initialState(List(
          Start[IO](console),
          Agenda[IO](console),
          slide
        )))
        .flatMap(Presentation.make[IO](console, _))
      _ <- presentation.start().start
      _ <- IO(handleInput(console, presentation)).flatten.foreverM
    } yield ()).map(_ => ExitCode.Success)
  }

  def handleInput(c: NConsole[IO], presentation: Presentation[IO]): IO[Input] = for {
    input <- c.read()
    _ <- presentation.userInput(input)
  } yield input

}
