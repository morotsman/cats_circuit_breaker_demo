package com.github.morotsman
package presentation

import cats.effect._
import presentation.slides.demo_slide.{CircuitBreakerSlide, ControlPanel, ControlPanelState, DemoProgramExecutor, DemoProgramExecutorState}
import presentation.slides.{Agenda, Start}
import presentation.tools.{NConsole, Presentation, PresentationState}
import presentation.demo.{MayhemState, SourceOfMayhem, Statistics, StatisticsState}
import presentation.slides.demo_slide.animations.{Animator, AnimatorState}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      console <- NConsole.make[IO]()
      circuitBreakerSlide <- createCircuitBreakerSlide(console)
      presentation <- Ref[IO]
        .of(PresentationState.make(List(
          Start[IO](console),
          Agenda[IO](console),
          circuitBreakerSlide
        )))
        .flatMap(Presentation.make[IO](console, _))
      _ <- presentation.start()
    } yield ()).map(_ => ExitCode.Success)
  }

  private def createCircuitBreakerSlide(console: NConsole[IO]): IO[CircuitBreakerSlide[IO]] = for {
    sourceOfMayhem <- Ref[IO].of(MayhemState.make()).map(SourceOfMayhem.make[IO])
    statistics <- Ref[IO].of(StatisticsState.make()).map(Statistics.make[IO])
    demoProgramExecutor <- Ref[IO].of(DemoProgramExecutorState.make[IO]()).flatMap(state => DemoProgramExecutor.make(
      state = state,
      sourceOfMayhem = sourceOfMayhem,
      statistics = statistics
    ))
    controlPanel <- Ref[IO].of(ControlPanelState.make[IO]()).map(state => ControlPanel.make[IO](
      state = state,
      sourceOfMayhem = sourceOfMayhem,
      demoProgramExecutor = demoProgramExecutor
    ))
    // TODO gather all info needed for animator in statistics
    animator <- Ref[IO].of(AnimatorState.make()).flatMap(state =>
      Animator.make[IO](state, controlPanel, statistics, sourceOfMayhem, demoProgramExecutor, console)
    )
    circuitBreakerSlide = CircuitBreakerSlide[IO](
      sourceOfMayhem,
      statistics,
      demoProgramExecutor,
      controlPanel,
      animator,
      console
    )
  } yield circuitBreakerSlide
}
