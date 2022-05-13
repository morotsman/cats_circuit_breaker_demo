package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.util.Colors

import com.github.morotsman.presentation.demo.StatisticsInfo

object Static {

  val tmp: () => String = () => "hepp"

  val staticAnimation = List(
    (s: StatisticsInfo) => raw"""
         |     ${s.circuitBreakerState}
         |     Program called last second: ${s.programCalledSinceLastReport}
         |     Request sent since last second: ${s.sentSinceLastReport}
         |     Pending requests: ${s.pendingRequests}
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
         |  Toggle fail: f                                        | |                                         | |                     | |
         |  Success latency: l +/-                                | |                                         | |                    \|_|/
         |  Timeout: t +/-                                        | |                                        fail                     \ /
         |  Failure threshold: a +/-                              | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
         |  Reset timeout: r +/-                                  | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |  Max reset timeout: m +/-                              | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
         |  Start/Stop: s                                         |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
         |  Number of requests: n +/-                                    Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
         |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
         |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
         |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
         |
         |
         |""".stripMargin
  )

}
