package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.util.Colors

import com.github.morotsman.presentation.demo.StatisticsInfo

object Static {

  val tmp: () => String = () => "hepp"

  def constantWidth(s: String, width: Int): String =
    if (s.length > width) {
      s.take(width)
    } else if (s.length < width) {
      s + (" " * (width - s.length))
    } else s

  val staticAnimation = List(
    (s: StatisticsInfo) => raw"""
         |     ${showCircuitBreakerState(s, 40)} hepp
         |     ${showProgramCalled(s, 40)} hepp
         |     ${showSourceOfMayhemCalled(s, 40)} hepp
         |     ${showPendingRequests(s, 40)} hepp
         |
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
         |  Start/Stop: s                                         | |                                         | |                     | |
         |  Number of requests: n +/-                             | |                                         | |                    \|_|/
         |  Toggle fail: f                                        | |                                        fail                     \ /
         |  Success latency: l +/-                                | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
         |  Timeout: t +/-                                        | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |  Failure threshold: a +/- Reset timeout: r +/-         | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
         |  Reset timeout: r +/-                                  |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
         |  Max reset timeout: m +/-                                     Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
         |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
         |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
         |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |
         |
         |""".stripMargin
  )

  private def showPendingRequests(s: StatisticsInfo, width: Int) = {
    constantWidth(s"Pending requests: ${s.pendingRequests}", width)
  }

  private def showSourceOfMayhemCalled(s: StatisticsInfo, width: Int) = {
    constantWidth(s"SourceOfMayhem called last second: ${s.sentSinceLastReport}", width)
  }

  private def showProgramCalled(s: StatisticsInfo, width: Int) = {
    constantWidth(s"Program called last second: ${s.programCalledSinceLastReport}", width)
  }

  private def showCircuitBreakerState(s: StatisticsInfo, width: Int) = {
    constantWidth(s"The Circuit breaker is ${s.circuitBreakerState.toString}", width)
  }
}
