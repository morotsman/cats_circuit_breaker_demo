package com.github.morotsman
package presentation.slides.demo_slide

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import presentation.demo.{DemoProgram, SourceOfMayhem, Statistics, StatisticsInfo}
import presentation.tools.{Character, Input, NConsole, Slide}

import com.github.morotsman.presentation.slides.demo_slide.animations.Static.staticAnimation

import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class DemoConfiguration(
                                    maxFailures: Int,
                                    resetTimeout: FiniteDuration,
                                    maxResetTimeout: FiniteDuration
                                  )

case class CircuitBreakerDemo[F[_] : Monad : Temporal : Spawn]
(
  console: NConsole[F],
  demoProgramFactory: (DemoConfiguration, SourceOfMayhem[F], Statistics[F]) => F[DemoProgram[F]],
  sourceOfMayhem: SourceOfMayhem[F],
  statistics: Statistics[F],
  state: Ref[F, CircuitBreakerDemoState[F]]
) extends Slide[F] {

  override def show(): F[Unit] = {
    for {
      animation <- animate(animation = staticAnimation).start
      statisticsPoller <- (forever(1.seconds) {
        for {
          info <- statistics.getStatisticsInfo()
          _ <- state.modify(s => (s.copy(
            statisticsInfo = info
          ), s))
        } yield ()
      }).start
      _ <- {
        state.modify(s => (s.copy(
          currentAnimation = Option(animation),
          statisticsPoller = Option(statisticsPoller)
        ), s))
      }
    } yield ()

  }

  override def userInput(input: Input): F[Unit] = for {
    _ <- input match {
      case Character(c) if c == 'f' =>
        sourceOfMayhem.toggleFailure()
      case Character(c) if c == 's' =>
        for {
          f <- (for {
            demoProgram <- demoProgramFactory(
              DemoConfiguration(
                maxFailures = 5,
                resetTimeout = 3.seconds,
                maxResetTimeout = 30.seconds
              ), sourceOfMayhem, statistics
            )
            _ <- forever(1.milli) {
              demoProgram.run().start
            }
          } yield ()).start
          _ <- {
            state.modify(s => (s.copy(
              demoProgram = Option(f)
            ), s))
          }
        } yield ()
      case _ =>
        Monad[F].unit
    }
  } yield true

  private def forever(delay: FiniteDuration)(effect: => F[_]): F[Unit] =
    Temporal[F].sleep(delay) >> effect >> forever(delay)(effect)

  private def animate(frame: Int = 0, animation: List[StatisticsInfo => String]): F[Unit] = {
    for {
      s <- state.get
      _ <- console.writeString(animation(frame)(s.statisticsInfo)) >>
        Temporal[F].sleep(500.milli) >>
        console.clear() >>
        animate(if (frame < animation.size - 1) frame + 1 else 0, animation)
    } yield ()
  }

  override def exit(): F[Unit] = for {
    s <- state.get
    _ <- s.currentAnimation.traverse(_.cancel)
    _ <- s.demoProgram.traverse(_.cancel)
    _ <- s.statisticsPoller.traverse(_.cancel)
  } yield ()

}
