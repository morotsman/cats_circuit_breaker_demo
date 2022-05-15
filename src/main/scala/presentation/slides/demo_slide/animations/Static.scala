package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.demo.{MayhemState, StatisticsInfo}
import presentation.tools.{Character, Input}
import presentation.util.Colors.{ANSI_GREEN, ANSI_RESET}
import presentation.slides.demo_slide.CircuitBreakerConfiguration

object Static {

  val staticAnimation = List(
    (
      s: StatisticsInfo,
      p: Option[Input],
      isStarted: Boolean,
      mayhemState: MayhemState,
      circuitBreakerConfiguration: CircuitBreakerConfiguration
    ) =>
      raw"""
           |     ${showCircuitBreakerState(s, 40)} ${showSuccessLatency(mayhemState, 40)} ${showProgramCalled(s, 40)} ${showAverageProgramCallTime(s, 40)}
           |     ${showThreshold(circuitBreakerConfiguration, 40)} ${showRequestTimeout(mayhemState, 40)} ${showSourceOfMayhemCalled(s, 40)} ${showAverageSourceOfMayhemCallTime(s, 40)}
           |     ${showResetTimeout(circuitBreakerConfiguration, 40)} ${showPendingRequests(s, 40)}
           |     ${showMaxResetTimeout(circuitBreakerConfiguration, 40)}
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
           |  ${toggleFailure(mayhemState, 40)}              | |                                         | |                    \|_|/
           |  ${numberOfRequests(p, 33)}                     | |                                        fail                     \ /
           |  ${successLatency(p, 31)}                       | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
           |  ${timeout(p, 24)}                              | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
           |  ${threshold(p, 26)}                            | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
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

  private def toggleFailure(mayhemState: MayhemState, width: Int): String =
    if (!mayhemState.isFailing) constantWidth("Toggle fail: f", width)
    else constantWidth(ANSI_GREEN + "Toggle fail: f" + ANSI_RESET, width + 9)

  private def numberOfRequests(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('n')).fold(constantWidth("Number of requests: n +/-", width)) { _ =>
      constantWidth(s"Number of requests: ${ANSI_GREEN + "n" + ANSI_RESET} +/-", width + 9)
    }
  }

  private def timeout(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('t')).fold(constantWidth("Timeout: t +/- ", width)) { _ =>
      constantWidth(s"Timeout: ${ANSI_GREEN + "t" + ANSI_RESET} +/-", width + 9)
    }
  }

  private def threshold(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('a')).fold(constantWidth("Failure threshold: a +/- ", width)) { _ =>
      constantWidth(s"Failure threshold: ${ANSI_GREEN + "a" + ANSI_RESET} +/-", width + 9)
    }
  }

  private def successLatency(previousInput: Option[Input], width: Int): String =
    previousInput.filter(_ == Character('l')).fold(constantWidth("Success latency: l +/-", width)) { _ =>
      constantWidth(s"Success latency: ${ANSI_GREEN + "l" + ANSI_RESET} +/-", width + 9)
    }

  private def showSuccessLatency(s: MayhemState, width: Int) =
    constantWidth(s"Success latency: ${s.successLatencyInMillis} ms", width)

  private def showRequestTimeout(s: MayhemState, width: Int) =
    constantWidth(s"Request timeout: ${s.requestTimeoutInMillis} ms", width)

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

  private def showThreshold(s: CircuitBreakerConfiguration, width: Int) =
    constantWidth(s"Threshold:  ${s.maxFailures} failure", width)

  private def showResetTimeout(s: CircuitBreakerConfiguration, width: Int) =
    constantWidth(s"Reset timeout:  ${s.resetTimeout.toSeconds} s", width)

  private def showMaxResetTimeout(s: CircuitBreakerConfiguration, width: Int) =
    constantWidth(s"Max reset timeout:  ${s.maxResetTimeout.toSeconds} s", width)

  def constantWidth(s: String, width: Int): String =
    if (s.length > width) {
      s.take(width)
    } else if (s.length < width) {
      s + (" " * (width - s.length))
    } else s
}
