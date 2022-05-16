package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.demo.{MayhemState, StatisticsInfo}
import presentation.tools.Input
import presentation.util.Colors.{ANSI_GREEN, ANSI_RESET}
import presentation.slides.demo_slide.CircuitBreakerConfiguration
import presentation.slides.demo_slide.animations.AnimationHelper._

object Static {

  val staticAnimation = List(
    (
      s: StatisticsInfo,
      p: Option[Input],
      isStarted: Boolean,
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
             |  ${startStop(isStarted, 34)}                    | |                                         | |                     | |
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
             |""".stripMargin
  )

  def showRuntimeInfo(
                       s: StatisticsInfo,
                       mayhemState: MayhemState,
                       circuitBreakerConfiguration: CircuitBreakerConfiguration
                     ) =
    raw"""
         |     ${showCircuitBreakerState(s, 40)} ${showSuccessLatency(mayhemState, 40)} ${showProgramCalled(s, 40)} ${showAverageProgramCallTime(s, 40)}
         |     ${showThreshold(circuitBreakerConfiguration, 40)} ${showRequestTimeout(mayhemState, 40)} ${showSourceOfMayhemCalled(s, 40)} ${showAverageSourceOfMayhemCallTime(s, 40)}
         |     ${showResetTimeout(circuitBreakerConfiguration, 40)} ${showPendingRequests(s, 40)}
         |     ${showMaxResetTimeout(circuitBreakerConfiguration, 40)}
         |
         |""".stripMargin


}
