package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

trait Presentation[F[_]] {
  def start(): F[Unit]
}

final case class PresentationState[F[_]](
                                          slideIndex: Int,
                                          slides: List[Slide[F]]
                                        )

object PresentationState {
  def make[F[_]](slides: List[Slide[F]]): PresentationState[F] =
    PresentationState[F](0, slides)
}

object Presentation {
  def make[F[_] : Temporal : Spawn]
  (
    console: NConsole[F],
    state: Ref[F, PresentationState[F]]
  ): F[Presentation[F]] = Monad[F].pure(
    new Presentation[F] {
      override def start(): F[Unit] = for {
        _ <- console.clear()
        currentState <- state.get
        f <- currentState.slides.head.show().start
        _ <- handleInput(f)
      } yield ()

      def handleInput(currentWork: Fiber[F, Throwable, Unit]): F[Input] = for {
        input <- console.read()
        work <- input match {
          case Key(k) if k == SpecialKey.Left =>
            for {
              _ <- currentWork.cancel
              f <- previousSlide().start
            } yield f
          case Key(k) if k == SpecialKey.Right =>
            for {
              _ <- currentWork.cancel
              f <- nextSlide().start
            } yield f
          case _ =>
            for {
              presentationState <- state.get
              slide = presentationState.slides(presentationState.slideIndex)
              _ <- slide.userInput(input)
            } yield currentWork
        }
        _ <- handleInput(work)
      } yield input

      private def previousSlide(): F[Unit] = for {
        updatedState <- state.updateAndGet(s =>
          if (s.slideIndex > 0) {
            s.copy(slideIndex = s.slideIndex - 1)
          } else {
            s
          }
        )
        _ <- console.clear()
        _ <- updatedState.slides(updatedState.slideIndex).show()
      } yield ()

      private def nextSlide(): F[Unit] = for {
        updatedState <- state.updateAndGet(s =>
          if (s.slideIndex == s.slides.size - 1) {
            s
          } else {
            s.copy(slideIndex = s.slideIndex + 1)
          }
        )
        _ <- console.clear()
        _ <- updatedState.slides(updatedState.slideIndex).show()
      } yield ()

    }
  )
}
