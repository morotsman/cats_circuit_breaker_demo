package com.github.morotsman
package presentation.slides.demo_slide

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import presentation.slides.demo_slide.animations.ClosedFailedUnderThreshold.closedFailedUnderThresholdAnimation
import presentation.slides.demo_slide.animations.ClosedSuccess.closedSuccessAnimation
import presentation.demo.{DemoProgram, SourceOfMayhem, Statistics}
import presentation.tools.{Character, Input, NConsole, Slide}

import com.github.morotsman.presentation.slides.demo_slide.animations.Static.staticAnimation

import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class CircuitBreakerDemoState[F[_]](
                                                currentAnimation: Option[Fiber[F, Throwable, Unit]]
                                        )

object CircuitBreakerDemoState {
  def initial[F[_]](): CircuitBreakerDemoState[F] = CircuitBreakerDemoState[F](
    currentAnimation = None
  )
}

final case class DemoConfiguration(
                                    maxFailures: Int,
                                    resetTimeout: FiniteDuration,
                                    maxResetTimeout: FiniteDuration
                                  )

case class CircuitBreakerDemo[F[_] : Monad : Temporal : Spawn](
                                                                console: NConsole[F],
                                                                demoProgramFactory: (DemoConfiguration, SourceOfMayhem[F], Statistics[F]) => F[DemoProgram[F]],
                                                                sourceOfMayhem: SourceOfMayhem[F],
                                                                statistics: Statistics[F],
                                                                state: Ref[F, CircuitBreakerDemoState[F]]
                                                              ) extends Slide[F] {

  override def show(): F[Unit] = {
    for {
      f <- animate(animation = closedSuccessAnimation).start
      _ <- {
        state.modify(s => (s.copy(
          currentAnimation = Option(f)
        ), s))
      }
    } yield ()

  }

  // poll for statistics, on change => update animation
  // receive user input s => start program
  // receive user input f => toggle error


  private def showStatistics(): F[Unit] = forever(1.seconds) {
    for {
      info <- statistics.getStatisticsInfo()
      _ <- console.writeString("statistics: " + info)
    } yield ()
  }

  /*
              forever(1.seconds) {
              for {
                info <- statistics.getStatisticsInfo()
                _ <- console.writeString("statistics: " + info)
              } yield ()
            }
   */

  override def userInput(input: Input): F[Unit] = for {
    _ <- input match {
      case Character(c) if c == 'f' =>
        sourceOfMayhem.toggleFailure()
      case Character(c) if c == 's' =>
        (for {
          demoProgram <- demoProgramFactory(
            DemoConfiguration(
              maxFailures = 5,
              resetTimeout = 3.seconds,
              maxResetTimeout = 30.seconds
            ), sourceOfMayhem, statistics
          )
          _ <- forever(5.micros) {
              demoProgram.run().start
            }
        } yield ()).start
      case _ =>
        Monad[F].unit
    }
  } yield true

  private def forever(delay: FiniteDuration)(effect: => F[_]): F[Unit] =
    Temporal[F].sleep(delay) >> effect >> forever(delay)(effect)

  private def animate(frame: Int = 0, animation: List[String]): F[Unit] =
    console.writeString(animation(frame)) >>
      Temporal[F].sleep(500.milli) >>
      console.clear() >>
      animate(if (frame < animation.size - 1) frame + 1 else 0, animation)

  override def exit(): F[Unit] = for {
    s <- state.get
    _ <- s.currentAnimation.traverse(_.cancel)
  } yield ()

}
