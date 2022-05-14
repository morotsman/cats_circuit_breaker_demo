package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.demo.StatisticsInfo
import presentation.tools.{Character, Input}

import com.github.morotsman.presentation.util
import com.github.morotsman.presentation.util.Colors.{ANSI_GREEN, ANSI_RESET}

object Static {

  val staticAnimation = List(
    (s: StatisticsInfo, p: Option[Input], isStarted: Boolean, isFailing: Boolean) =>
      raw"""
           |     ${showCircuitBreakerState(s, 40)}
           |     ${showProgramCalled(s, 40)} ${showAverageProgramCallTime(s, 40)}
           |     ${showSourceOfMayhemCalled(s, 40)} ${showAverageSourceOfMayhemCallTime(s, 40)}
           |     ${showPendingRequests(s, 40)}
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
           |  ${startStop(isStarted, 34)}                    | |                                         | |                     | |
           |  ${toggleFailure(isFailing, 38)}                | |                                         | |                    \|_|/
           |  ${numberOfRequests(p, 33)}                     | |                                        fail                     \ /
           |  Success latency: l +/-                                | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
           |  Timeout: t +/-                                        | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
           |  Failure threshold: a +/-                              | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
           |  Reset timeout: r +/-                                  |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
           |  Max reset timeout: m +/-                                     Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
           |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
           |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
           |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
           |
           |
           |""".stripMargin
  )

  private def startStop(isStarted: Boolean, width: Int): String =
    if (!isStarted) constantWidth("Start/Stop: s", width)
    else constantWidth(ANSI_GREEN + "Start/Stop: s" + ANSI_RESET, width + 9)

  private def toggleFailure(isFailing: Boolean, width: Int): String =
    if (!isFailing) constantWidth("Toggle fail: f", width)
    else constantWidth(ANSI_GREEN + "Toggle fail: f" + ANSI_RESET, width + 9)

  private def numberOfRequests(previousInput: Option[Input], width: Int): String =
    previousInput.fold(constantWidth("Number of requests: n +/-", width)) { input =>
      constantWidth(s"Number of requests: ${if (input == Character('n')) (ANSI_GREEN + "n" + ANSI_RESET) else "n"} +/-", width + 9)
    }

  private def showAverageProgramCallTime(s: StatisticsInfo, width: Int) =
    if (s.programCompletedIn.nonEmpty) {
      constantWidth(s"Average time: ${s.programCompletedIn.sum / s.programCompletedIn.length} ms", width)
    } else {
      constantWidth(s"Average time: 0 ms", width)
    }

  private def showAverageSourceOfMayhemCallTime(s: StatisticsInfo, width: Int) =
    if (s.requestsCompletedIn.nonEmpty) {
      constantWidth(s"Average time: ${s.requestsCompletedIn.sum / s.requestsCompletedIn.length} ms", width)
    } else {
      constantWidth(s"Average time: 0 ms", width)
    }

  private def showPendingRequests(s: StatisticsInfo, width: Int) =
    constantWidth(s"Pending requests: ${s.pendingRequests}", width)

  private def showSourceOfMayhemCalled(s: StatisticsInfo, width: Int) =
    constantWidth(s"SourceOfMayhem called last second: ${s.sentSinceLastReport}", width)

  private def showProgramCalled(s: StatisticsInfo, width: Int) =
    constantWidth(s"Program called last second: ${s.programCalledSinceLastReport}", width)

  private def showCircuitBreakerState(s: StatisticsInfo, width: Int) =
    constantWidth(s"The Circuit breaker is ${s.circuitBreakerState.toString}", width)

  def constantWidth(s: String, width: Int): String =
    if (s.length > width) {
      s.take(width)
    } else if (s.length < width) {
      s + (" " * (width - s.length))
    } else s
}
