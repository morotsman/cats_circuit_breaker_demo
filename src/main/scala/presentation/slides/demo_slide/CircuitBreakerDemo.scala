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


import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class CircuitBreakerDemoState(

                                        )

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
                                                                state: Ref[F, CircuitBreakerDemoState]
                                                              ) extends Slide[F] {

  override def show(): F[Unit] = {
    val tmp = forever(1.seconds) {
      for {
        info <- statistics.getStatisticsInfo()
        _ <- console.writeString("statistics: " + info)
      } yield ()
    }



    animate(animation = closedSuccessAnimation)
  }

  private def showStatistics(): F[Unit] = forever(1.seconds) {
    for {
      info <- statistics.getStatisticsInfo()
      _ <- console.writeString("statistics: " + info)
    } yield ()
  }

  override def userInput(input: Input): F[Boolean] = for {
    _ <- console.clear()
    _ <- input match {
      case Character(c) if c == 'f' =>
        animate(animation = closedFailedUnderThresholdAnimation).map(_ => true)
      case Character(c) if c == 's' =>
        for {
          demoProgram <- demoProgramFactory(
            DemoConfiguration(
              maxFailures = 5,
              resetTimeout = 3.seconds,
              maxResetTimeout = 30.seconds
            ), sourceOfMayhem, statistics
          )
          _ <- (
            forever(5.micros) {
              demoProgram.run().start
            },
            forever(1.seconds) {
              for {
                info <- statistics.getStatisticsInfo()
                _ <- console.writeString("statistics: " + info)
              } yield ()
            }
            ).parTupled
        } yield ()
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

}



/*
      """
        |           __   Success                                                                                   __  call / raise circuit open
        |        _ / /__ ___ ___ ___ ___ _                                                                      _ / /__ ___ ___ ___ ___ _
        |       | < <___|___|___|___|___| |                                                                    | < <___|___|___|___|___| |
        |       | |\_\                  | |                                                                    | |\_\                  | |
        |       | |                     | |                                                                    | |                     | |
        |       | |                     | |                                                                    | |                     | |
        |  ___ _|_|___ ___ ___ ___ ___ _|_|___ ___           fail (threshold reached)               __    ___ _|_|___ ___ ___ ___ ___ _|_|___ ___
        | |___|___|___|___|___|___|___|___|___|___|      ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___\ \  |___|___|___|___|___|___|___|___|___|___|
        | |_|     ___ _    ___  ___ ___ ___     |_|      ___|___|___|___|___|___|___|___|___|___|___ > > |_|         ___  ___ ___ _  _         |_|
        | | |    / __| |  / _ \/ __| __|   \    | |     __                                          /_/  | |        / _ \| _ \ __| \| |        | |
        | | |   | (__| |_| (_) \__ \ _|| |) |   | |    / /___ ___ _                                      | |       | (_) |  _/ _|| .` |        | |
        | | |    \___|____\___/|___/___|___/    | |   < < ___|___| |                                     | |        \___/|_| |___|_|\_|        | |
        | |_|_ ___ ___ ___ ___ ___ ___ ___ ___ _|_|    \_\       | |                                     |_|_ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
        | |___|___|___|___|___|___|___|___|___|___|              | |                                     |___|___|___|___|___|___|___|___|___|___|
        |     | |                     | |                        | |
        |     | | __                  | |                        | |                                         / \                 reset timeout
        |     | |/ /__ ___ ___ ___ ___| |                        | |                                        /| |\                    | |
        |     |_< <___|___|___|___|___|_|                        | |                                         | |                     | |
        |        \_\                                             | |                                         | |                     | |
        |           fail (under threshold)                       | |                                         |_|                     |_|
        |                                                        | |                                         | |                     | |
        |                                                        | |                                         | |                     | |
        |                                                        | |                                         | |                    \|_|/
        |                                                        | |                                        fail                     \ /
        |                                                        | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
        |                                                        | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
        |                                                        | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
        |                                                        |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
        |                                                               Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
        |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
        |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
        |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
        |
        |
        |"""
 */

