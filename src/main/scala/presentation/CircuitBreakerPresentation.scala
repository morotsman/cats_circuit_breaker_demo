package com.github.morotsman
package presentation

import cats.implicits._
import cats.effect._

import scala.concurrent.duration.DurationInt

object CircuitBreakerPresentation extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      console <- Console.make[IO]()
      presentation <- Ref[IO]
        .of(PresentationState(0, List(
          Slide1[IO](console),
          Slide2[IO](console),
          Slide3[IO](console),
          Slide4[IO](console)
        )))
        .flatMap(Presentation.make[IO](console, _))
      _ <-
        for {
          _ <- presentation.start()
          _ <- IO(handleInput(console, presentation)).flatten.foreverM
        } yield ()
    } yield ()).map(_ => ExitCode.Success)
  }

  def handleInput(c: Console[IO], presentation: Presentation[IO]): IO[Input] = for {
    input <- c.read()
    _ <- input match {
      case Key(k) if k == SpecialKey.Left => presentation.previousSlide()
      case Key(k) if k == SpecialKey.Right => presentation.nextSlide()
      case Key(k) if k == SpecialKey.Esc => presentation.exit()
      case _ => ???
    }
  } yield input

}
