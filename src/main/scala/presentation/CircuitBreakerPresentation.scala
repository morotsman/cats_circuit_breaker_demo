package com.github.morotsman
package presentation

import cats.effect._

import scala.concurrent.duration.DurationInt

object CircuitBreakerPresentation extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      console <- Console.make[IO]()
      presentation <- Ref[IO]
        .of(PresentationState(0, List(
          Start[IO](console),
          Agenda[IO](console),
          StaticViewCircuitBreaker[IO](console),
          CircuitBreakerDemo[IO](console)
        )))
        .flatMap(Presentation.make[IO](console, _))
      firstSlide <- presentation.start().start
      ref <- Ref[IO].of(CurrentSlide(firstSlide))
      _ <- IO(handleInput(console, presentation, ref)).flatten.foreverM
    } yield ()).map(_ => ExitCode.Success)
  }

  case class CurrentSlide(fiber: FiberIO[Unit])

  def handleInput(c: Console[IO], presentation: Presentation[IO], ref: Ref[IO, CurrentSlide]): IO[Input] = for {
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
      case _ => presentation.userInput(input).start
    }
  } yield input

}
