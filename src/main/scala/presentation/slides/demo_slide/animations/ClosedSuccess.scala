package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.demo.{MayhemState, StatisticsInfo}
import presentation.slides.demo_slide.CircuitBreakerConfiguration
import presentation.slides.demo_slide.animations.AnimationHelper._
import presentation.tools.Input
import presentation.util.Colors._

object ClosedSuccess {

  val ClosedSuccessAnimation = List(
    (
      s: StatisticsInfo,
      p: Option[Input],
      mayhemState: MayhemState,
      circuitBreakerConfiguration: CircuitBreakerConfiguration
    ) =>
      showRuntimeInfo(s, mayhemState, circuitBreakerConfiguration) +
    raw"""
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
         |  ${toggleFailure(mayhemState, 40)}              | |                                         | |                    \|_|/
         |  ${numberOfRequests(p, 33)}                     | |                                        fail                     \ /
         |  ${successLatency(p, 31)}                       | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
         |  ${timeout(p, 24)}                              | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |  ${threshold(p, 26)}                            | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
         |  ${resetTimeout(p, 29)}                         |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
         |  ${maxResetTimeout(p, 32)}                             Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
         |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
         |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
         |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |
         |
         |""".replaceAll("1", ANSI_GREEN + "*" + ANSI_RESET).stripMargin
  )

}
