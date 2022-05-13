package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.implicits._

trait Presentation[F[_]] {
  def start(): F[Unit]

  def nextSlide(): F[Unit]

  def previousSlide(): F[Unit]

  def exit(): F[Unit]

  def userInput(input: Input): F[Unit]
}

final case class PresentationState[F[_]](
                                          slideIndex: Int,
                                          slides: List[Slide[F]]
                                        )

object PresentationState {
  def initialState[F[_]](slides: List[Slide[F]]): PresentationState[F] =
    PresentationState[F](0, slides)
}

object Presentation {
  def make[F[_] : Monad : Temporal : Spawn](
                         console: NConsole[F],
                         state: Ref[F, PresentationState[F]]
                       ): F[Presentation[F]] = Monad[F].pure(
    new Presentation[F] {
      override def start(): F[Unit] = for {
        _ <- console.clear()
        state <- state.get
        _ <- state.slides.head.show()
      } yield ()

      override def nextSlide(): F[Unit]= for {
        _ <- console.clear()
        state <- state.updateAndGet(s =>
          if (s.slideIndex == s.slides.size - 1) {
            s
          } else {
            s.copy(slideIndex = s.slideIndex + 1)
          }
        )
        _ <- state.slides(state.slideIndex).show()
      } yield ()

      override def previousSlide(): F[Unit] = for {
        _ <- console.clear()
        state <- state.updateAndGet(s =>
          if (s.slideIndex > 0) {
            s.copy(slideIndex = s.slideIndex - 1)
          } else {
            s
          }
        )
        f <- state.slides(state.slideIndex).show()
      } yield f

      override def exit(): F[Unit] = ???

      override def userInput(input: Input): F[Unit] = for {
        presentationState <- state.get
        _ <- input match {
          case Key(k) if k == SpecialKey.Left =>
            for {
              _ <- presentationState.slides(presentationState.slideIndex).exit()
              _ <- previousSlide()
            } yield ()
          case Key(k) if k == SpecialKey.Right =>
            for {
              _ <- presentationState.slides(presentationState.slideIndex).exit()
              _ <- nextSlide()
            } yield ()
          case _ =>
            for {
              presentationState <- state.get
              slide = presentationState.slides(presentationState.slideIndex)
              _ <- slide.userInput(input)
            } yield ()
        }
      } yield ()

    }
  )
}
