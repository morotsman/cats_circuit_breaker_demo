package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import presentation.tools.DebugHelper.DebugHelper

trait Presentation[F[_]] {
  def start(): F[Unit]
}

object Presentation {
  def make[F[_] : Temporal : Spawn]
  (slides: List[Slide[F]])(implicit C: NConsole[F]): F[Presentation[F]] = Monad[F].pure(
    new Presentation[F] {
      override def start(): F[Unit] = for {
        _ <- C.clear()
        f <- slides.head.show().start
        _ <- executionLoop(f)
      } yield ()

      def executionLoop(currentWork: Fiber[F, Throwable, Unit], currentSlideIndex: Int = 0): F[Unit] = for {
        input <- C.read()
        (slide, work) <- input match {
          case Key(k) if k == SpecialKey.Left =>
            if (currentSlideIndex > 0) {
              for {
                _ <- currentWork.cancel
                _ <- C.clear()
                index = currentSlideIndex - 1
                newWork <- slides(index).show().start
              } yield (index, newWork)
            } else {
              Monad[F].pure((currentSlideIndex, currentWork))
            }
          case Key(k) if k == SpecialKey.Right =>
            if (currentSlideIndex < slides.length - 1) {
              for {
                _ <- currentWork.cancel
                _ <- C.clear()
                index = currentSlideIndex + 1
                newWork <- slides(index).show().start
              } yield (index, newWork)
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
