package com.github.morotsman
package presentation.slides.demo_slide

import cats._
import cats.effect._
import cats.implicits._
import presentation.demo.SourceOfMayhem
import presentation.tools.{Character, Input, Slide}

case class ControlPanel[F[_] : Temporal : Spawn]
(
  state: Ref[F, ControlPanelState[F]],
  sourceOfMayhem: SourceOfMayhem[F],
  demoProgramExecutor: DemoProgramExecutor[F]
) extends Slide[F] {

  def getState(): F[ControlPanelState[F]] =
    state.get

  override def show(): F[Unit] = {
    Monad[F].unit
  }

  override def userInput(input: Input): F[Unit] = for {
    _ <- input match {
      case Character(c) if c == 'f' =>
        sourceOfMayhem.toggleFailure() >> state.modify(s => (s.copy(
          isFailing = !s.isFailing
        ), s))
      case Character(c) if c == 's' =>
        ???
      case Character(c) if c == 'n' =>
        state.modify(s => (s.copy(
          previousInput = Option(input)
        ), s))
      case Character(c) if c == 'l' =>
        state.modify(s => (s.copy(
          previousInput = Option(input)
        ), s))
      case Character(c) if c == 't' =>
        state.modify(s => (s.copy(
          previousInput = Option(input)
        ), s))
      case Character(c) if c == 'a' =>
        state.modify(s => (s.copy(
          previousInput = Option(input)
        ), s))
      case Character(c) if c == 'r' =>
        state.modify(s => (s.copy(
          previousInput = Option(input)
        ), s))
      case Character(c) if c == 'm' =>
        state.modify(s => (s.copy(
          previousInput = Option(input)
        ), s))
      case Character(c) if c == '+' =>
        for {
          s <- state.get
          _ <- s.previousInput.traverse {
            case Character(c) if c == 'n' =>
              demoProgramExecutor.decreaseDelayBetweenCallsToSourceOfMayhem()
            case Character(c) if c == 'l' =>
              sourceOfMayhem.increaseSuccessLatency()
            case Character(c) if c == 't' =>
              sourceOfMayhem.increaseRequestTimeout()
            case Character(c) if c == 'a' =>
              demoProgramExecutor.increaseMaxFailures()
            case Character(c) if c == 'r' =>
              demoProgramExecutor.increaseResetTimeout()
            case Character(c) if c == 'm' =>
              demoProgramExecutor.increaseMaxResetTimeout()
            case _ =>
              Monad[F].unit
          }
        } yield ()
      case Character(c) if c == '-' =>
        for {
          s <- state.get
          _ <- s.previousInput.traverse {
            case Character(c) if c == 'n' =>
              demoProgramExecutor.increaseDelayBetweenCallsToSourceOfMayhem()
            case Character(c) if c == 'l' =>
              sourceOfMayhem.decreaseSuccessLatency()
            case Character(c) if c == 't' =>
              sourceOfMayhem.decreaseRequestTimeout()
            case Character(c) if c == 'a' =>
              demoProgramExecutor.decreaseMaxFailures()
            case Character(c) if c == 'r' =>
              demoProgramExecutor.decreaseResetTimeout()
            case Character(c) if c == 'm' =>
              demoProgramExecutor.increaseMaxResetTimeout()
            case _ =>
              Monad[F].unit
          }
        } yield ()

      case _ =>
        Monad[F].unit
    }
  } yield true

  override def exit(): F[Unit] = for {
    s <- state.get
  } yield ()

}
