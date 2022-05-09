package com.github.morotsman
package presentation

import cats.implicits._
import cats.effect._

trait Presentation[F[_]] {
  def start(): F[Unit]

  def nextSlide(): F[Unit]

  def previousSlide(): F[Unit]

  def exit(): F[Unit]
}

final case class PresentationState[F[_]](
                                          slideIndex: Int,
                                          slides: List[Slide[F]]
                                        )

object Presentation {
  def make[F[_] : Sync](
                         console: Console[F],
                         state: Ref[F, PresentationState[F]]
                       ): F[Presentation[F]] = Sync[F].delay(
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


    }
  )
}
