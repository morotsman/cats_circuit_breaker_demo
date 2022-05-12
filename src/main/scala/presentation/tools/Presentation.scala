package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

trait Presentation[F[_]] {
  def start(): F[Fiber[F, Throwable, Unit]]

  def nextSlide(): F[Fiber[F, Throwable, Unit]]

  def previousSlide(): F[Fiber[F, Throwable, Unit]]

  def exit(): F[Fiber[F, Throwable, Unit]]

  def userInput(input: Input): F[Fiber[F, Throwable, Unit]]
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
      override def start(): F[Fiber[F, Throwable, Unit]] = for {
        _ <- console.clear()
        state <- state.get
        f <- state.slides.head.show()
      } yield f

      override def nextSlide(): F[Fiber[F, Throwable, Unit]]= for {
        _ <- console.clear()
        state <- state.updateAndGet(s =>
          if (s.slideIndex == s.slides.size - 1) {
            s
          } else {
            s.copy(slideIndex = s.slideIndex + 1)
          }
        )
        f <- state.slides(state.slideIndex).show()
      } yield f

      override def previousSlide(): F[Fiber[F, Throwable, Unit]] = for {
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

      override def exit(): F[Fiber[F, Throwable, Unit]] = ???

      override def userInput(input: Input): F[Fiber[F, Throwable, Unit]] = for {
        presentationState <- state.get
        f <- input match {
          case Key(k) if k == SpecialKey.Left =>
            for {
              _ <- presentationState.ongoingWork.traverse(_.cancel)
              f <- previousSlide()
              _ <- state.modify(s =>
                  (s.copy(
                    ongoingWork = Option(f)
                  ), s))
            } yield f
          case Key(k) if k == SpecialKey.Right =>
            for {
              _ <- presentationState.ongoingWork.traverse(_.cancel)
              f <- nextSlide()
              _ <- state.modify(s =>
                (s.copy(
                  ongoingWork = Option(f)
                ), s))
            } yield f
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
            } yield f
        }
      } yield f

    }
  )
}
