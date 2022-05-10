package com.github.morotsman
package presentation

import cats.{FlatMap, Monad}
import cats.effect.Temporal
import cats.implicits._
import com.github.morotsman.presentation.demo.{DemoProgram, SourceOfMayhem, Statistics}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class DemoConfiguration(
                                    maxFailures: Int,
                                    resetTimeout: FiniteDuration,
                                    maxResetTimeout: FiniteDuration
                                  )

case class CircuitBreakerDemo[F[_] : Monad : Temporal](
                                                          console: NConsole[F],
                                                          demoProgramFactory: (DemoConfiguration, SourceOfMayhem[F], Statistics[F]) => F[DemoProgram[F]],
                                                          sourceOfMayhem: SourceOfMayhem[F],
                                                          statistics: Statistics[F]
                                                        ) extends Slide[F] {
  val test = 1000

  val closedFailedUnderThresholdAnimation = List(
    raw"""
         |           __   Success                                 Status: CLOSED                                    __  call / raise circuit open
         |        _ / /__ ___ ___ ___ ___ _                       Circuit breaker called:                        _ / /__ ___ ___ ___ ___ _
         |       | < <___|___|___|___|___| |                      Request per second:                           | < <___|___|___|___|___| |
         |       | |\_\                  | |                      Pending requests:                             | |\_\                  | |
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
         |  Toggle fail: f                                        | |                                         | |                     | |
         |  Success latency: s +/-                                | |                                         | |                    \|_|/
         |  Timeout: t +/-                                        | |                                        fail                     \ /
         |  Failure threshold: a +/-                              | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
         |  Reset timeout: r +/-                                  | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |  Max reset timeout: m +/-                              | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
         |  Start/Stop: s                                         |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
         |                                                               Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
         |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
         |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
         |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |
         |
         |""".stripMargin,
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
      |     | |                     |1|                        | |
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
      |""".stripMargin.replaceAll("1", Colors.ANSI_YELLOW + "*" + Colors.ANSI_RESET)
  )

  val closedSuccessAnimation = List(
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
      |""".stripMargin,
    """
      |           __   Success                                                                                   __  call / raise circuit open
      |        _ / /__ ___ ___ ___ ___ _                                                                      _ / /__ ___ ___ ___ ___ _
      |       | < <___|___|___|___|___| |                                                                    | < <___|___|___|___|___| |
      |       | |\_\                  | |                                                                    | |\_\                  | |
      |       | |                     | |                                                                    | |                     | |
      |       | |                     |1|                                                                    | |                     | |
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
      |""".stripMargin.replaceAll("1", Colors.ANSI_GREEN + "*" + Colors.ANSI_RESET)
  )


  override def show(): F[Unit] = {
    animate(animation = closedSuccessAnimation)
  }

  override def userInput(input: Input): F[Boolean] = for {
    _ <- console.clear()
    result <- input match {
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
          _ <- demoProgram.run()
          info <- statistics.getStatisticsInfo()
          _ <- Temporal[F].sleep(5.seconds) >> console.writeString("statistics: " + info)
        } yield true
      case _ =>
        Monad[F].pure(false)
    }
  } yield result

  private def animate(frame: Int = 0, animation: List[String]): F[Unit] = for {
    _ <- console.writeString(animation(frame))
    _ <- Temporal[F].sleep(500.milli)
    _ <- console.clear()
    _ <- animate(if (frame < animation.size - 1) frame + 1 else 0, animation)
  } yield ()
}



