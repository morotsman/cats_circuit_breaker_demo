package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
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
                                          slides: List[Slide[F]],
                                          currentSlide: Option[Fiber[F, Throwable, Unit]]
                                        )

object PresentationState {
  def make[F[_]](slides: List[Slide[F]]): PresentationState[F] =
    PresentationState[F](0, slides, None)
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
        _ <- state.modify(s => (s.copy(
          currentSlide = Option(f)
        ),s))
      } yield ()

      override def nextSlide(): F[Unit] = for {
        updatedState <- state.updateAndGet(s =>
          if (s.slideIndex == s.slides.size - 1) {
            s
          } else {
            s.copy(slideIndex = s.slideIndex + 1)
          }
        )
        _ <- updatedState.currentSlide.traverse(_.cancel)
        _ <- console.clear()
        f <- updatedState.slides(updatedState.slideIndex).show().start
        _ <- state.modify(s => (s.copy(
          currentSlide = Option(f)
        ),s))
      } yield ()

      override def previousSlide(): F[Unit] = for {
        updatedState <- state.updateAndGet(s =>
          if (s.slideIndex > 0) {
            s.copy(slideIndex = s.slideIndex - 1)
          } else {
            s
          }
        )
        _ <- updatedState.currentSlide.traverse(_.cancel)
        _ <- console.clear()
        f <- updatedState.slides(updatedState.slideIndex).show().start
        _ <- state.modify(s => (s.copy(
          currentSlide = Option(f)
        ),s))
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
