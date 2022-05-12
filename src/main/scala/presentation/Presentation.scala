package com.github.morotsman
package presentation

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

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
                                          ongoingWork: Option[Fiber[F, Throwable, Unit]]
                                        )

object PresentationState {
  def initialState[F[_]](slides: List[Slide[F]]): PresentationState[F] =
    PresentationState[F](0, slides, None)
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

      override def nextSlide(): F[Unit] = for {
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
        _ <- state.slides(state.slideIndex).show()
      } yield ()

      override def exit(): F[Unit] = ???

      override def userInput(input: Input): F[Unit] = for {
        presentationState <- state.get
        _ <- input match {
          case Key(k) if k == SpecialKey.Left =>
            for {
              _ <- presentationState.ongoingWork.traverse(_.cancel)
              f <- previousSlide().start
              _ <- state.modify(s =>
                  (s.copy(
                    ongoingWork = Option(f)
                  ), s))
            } yield ()
          case Key(k) if k == SpecialKey.Right =>
            for {
              _ <- presentationState.ongoingWork.traverse(_.cancel)
              f <- nextSlide().start
              _ <- state.modify(s =>
                (s.copy(
                  ongoingWork = Option(f)
                ), s))
            } yield ()
          case _ =>
            for {
              presentationState <- state.get
              slide = presentationState.slides(presentationState.slideIndex)
              _ <- presentationState.ongoingWork.traverse(_.cancel)
              f <- slide.userInput(input).start
              _ <- state.modify(s =>
                (s.copy(
                  ongoingWork = Option(f)
                ), s))
            } yield ()
        }
      } yield ()

    }
  )
}
