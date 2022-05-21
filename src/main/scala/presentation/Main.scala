package com.github.morotsman
package presentation

import cats.effect._
import presentation.slides.demo_slide.{CircuitBreakerSlide, ControlPanel, ControlPanelState, DemoProgramExecutor, DemoProgramExecutorState}
import presentation.slides.{Agenda, Start}
import presentation.tools.{NConsole, Presentation}
import presentation.demo.{MayhemState, SourceOfMayhem, Statistics, StatisticsState}
import presentation.slides.demo_slide.animations.{Animator, AnimatorState}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = for {
      console <- NConsole.make[IO]()
      circuitBreakerSlide <- createCircuitBreakerSlide(console)
      presentation <- Presentation.make[IO](console, List(
        Start[IO](console),
        Agenda[IO](console),
        circuitBreakerSlide
      ))
      _ <- presentation.start()
    } yield ()

  private def createCircuitBreakerSlide(console: NConsole[IO]): IO[CircuitBreakerSlide[IO]] = for {
    sourceOfMayhem <- Ref[IO].of(MayhemState.make()).map(SourceOfMayhem.make[IO])
    statistics <- Ref[IO].of(StatisticsState.make()).map(Statistics.make[IO])
    demoProgramExecutor <- Ref[IO].of(DemoProgramExecutorState.make[IO]()).flatMap(DemoProgramExecutor.make(
      _,
      sourceOfMayhem,
      statistics
    ))
    controlPanel <- Ref[IO].of(ControlPanelState.make[IO]()).map(ControlPanel.make[IO](
      _,
      sourceOfMayhem,
      demoProgramExecutor
    ))
    // TODO gather all info needed for animator in statistics
    animator <- Ref[IO].of(AnimatorState.make()).map(
      Animator.make[IO](_, statistics, sourceOfMayhem, demoProgramExecutor, console)
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
