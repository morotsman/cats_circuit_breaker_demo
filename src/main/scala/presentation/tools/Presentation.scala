package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

trait Presentation[F[_]] {
  def start(): F[Unit]
}

object Presentation {
  def make[F[_] : Temporal : Spawn]
  (
    console: NConsole[F],
    slides: List[Slide[F]],
  ): F[Presentation[F]] = Monad[F].pure(
    new Presentation[F] {
      override def start(): F[Unit] = for {
        _ <- console.clear()
        f <- slides.head.show().start
        _ <- executionLoop(f)
      } yield ()

      def executionLoop(currentWork: Fiber[F, Throwable, Unit], currentSlideIndex: Int = 0): F[Unit] = for {
        input <- console.read()
        (slide, work) <- input match {
          case Key(k) if k == SpecialKey.Left =>
            if (currentSlideIndex > 0) {
              for {
                _ <- currentWork.cancel
                _ <- console.clear()
                index = currentSlideIndex - 1
                f <- slides(index).show().start
              } yield (index, f)
            } else {
              Monad[F].pure((currentSlideIndex, currentWork))
            }
          case Key(k) if k == SpecialKey.Right =>
            if (currentSlideIndex < slides.length - 1) {
              for {
                _ <- currentWork.cancel
                _ <- console.clear()
                index = currentSlideIndex + 1
                f <- slides(index).show().start
              } yield (index, f)
            } else {
              Monad[F].pure((currentSlideIndex, currentWork))
            }
          case _ =>
            slides(currentSlideIndex).userInput(input).as((currentSlideIndex, currentWork))
        }
        _ <- executionLoop(work, slide)
      } yield ()

    }
  )
}
