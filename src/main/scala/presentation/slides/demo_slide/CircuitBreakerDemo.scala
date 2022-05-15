package com.github.morotsman
package presentation.slides.demo_slide

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import presentation.demo.{CircuitBreakerState, DemoProgram, MayhemState, SourceOfMayhem, Statistics, StatisticsInfo}
import presentation.tools.{Character, Input, NConsole, Slide}
import presentation.slides.demo_slide.animations.Static.staticAnimation

import io.chrisdavenport.circuit.{Backoff, CircuitBreaker}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class CircuitBreakerDemo[F[_] : Monad : Temporal : Spawn]
(
  console: NConsole[F],
  sourceOfMayhem: SourceOfMayhem[F],
  statistics: Statistics[F],
  state: Ref[F, CircuitBreakerDemoState[F]]
) extends Slide[F] {

  override def start(): F[Unit] = {
    for {
      s <- state.get
      animation <- animate(animation = staticAnimation).start
      statisticsPoller <- forever(1.seconds) {
        for {
          info <- statistics.getStatisticsInfo()
          _ <- state.modify(s => (s.copy(
            statisticsInfo = info
          ), s))
        } yield ()
      }.start
      demoProgram <- createDemoProgram(s.circuitBreakerConfiguration)
      _ <- {
        state.modify(s => (s.copy(
          currentAnimation = Option(animation),
          statisticsPoller = Option(statisticsPoller),
          demoProgram = Option(demoProgram)
        ), s))
      }
    } yield ()
  }

  private def createDemoProgram(circuitBreakerConfiguration: CircuitBreakerConfiguration): F[DemoProgram[F]] =
    CircuitBreaker.of[F](
      maxFailures = circuitBreakerConfiguration.maxFailures,
      resetTimeout = circuitBreakerConfiguration.resetTimeout,
      backoff = Backoff.exponential,
      maxResetTimeout = circuitBreakerConfiguration.maxResetTimeout,
      onOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.OPEN),
      onClosed = statistics.circuitBreakerStateChange(CircuitBreakerState.CLOSED),
      onRejected = MonadError[F, Throwable].unit,
      onHalfOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.HALF_OPEN)
    ).map { circuitBreaker =>
      DemoProgram[F](
        sourceOfMayhem = sourceOfMayhem,
        circuitBreaker = circuitBreaker,
        statistics = statistics
      )
    }

  override def userInput(input: Input): F[Unit] = for {
    _ <- input match {
      case Character(c) if c == 'f' =>
        sourceOfMayhem.toggleFailure() >> state.modify(s => (s.copy(
          isFailing = !s.isFailing
        ), s))
      case Character(c) if c == 's' =>
        for {
          s <- state.get
          _ <- if (s.demoProgramExecutor.isDefined) {
            s.demoProgramExecutor.traverse(_.cancel) >> state.modify(s => (s.copy(
              demoProgramExecutor = None,
              isStarted = false
            ), s))
          } else {
            updateProgramExecutor() >> state.modify(s => (s.copy(
              isStarted = true
            ), s))
          }
        } yield ()
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
            case Character(c) if c == 'n' && s.isStarted =>
              for {
                _ <- state.modify(s => (s.copy(
                  demoConfiguration = s.demoConfiguration.copy(
                    delayBetweenCallToSourceOfMayhemInNanos =
                      s.demoConfiguration.delayBetweenCallToSourceOfMayhemInNanos / 5
                  )
                ),
                  s))
                _ <- updateProgramExecutor()
              } yield ()
            case Character(c) if c == 'l' =>
              sourceOfMayhem.increaseSuccessLatency()
            case Character(c) if c == 't' =>
              sourceOfMayhem.increaseRequestTimeout()
            case Character(c) if c == 'a' && s.isStarted =>
              for {
                updatedState <- state.updateAndGet(s => s.copy(
                  circuitBreakerConfiguration = s.circuitBreakerConfiguration.copy(
                    maxFailures = s.circuitBreakerConfiguration.maxFailures * 2
                  )
                ))
                demoProgram <- createDemoProgram(updatedState.circuitBreakerConfiguration)
                _ <- state.modify(s => (s.copy(
                  demoProgram = Option(demoProgram)
                ), s))
                _ <- updateProgramExecutor()
              } yield ()
            case Character(c) if c == 'r' =>
              ???
            case Character(c) if c == 'm' =>
              ???
            case _ =>
              Monad[F].unit
          }
        } yield ()
      case Character(c) if c == '-' =>
        for {
          s <- state.get
          _ <- s.previousInput.traverse {
            case Character(c) if c == 'n' && s.isStarted =>
              for {
                _ <- state.modify(s => (s.copy(
                  demoConfiguration = s.demoConfiguration.copy(
                    delayBetweenCallToSourceOfMayhemInNanos =
                      s.demoConfiguration.delayBetweenCallToSourceOfMayhemInNanos * 5
                  )
                ),
                  s))
                _ <- updateProgramExecutor()
              } yield ()
            case Character(c) if c == 'l' =>
              sourceOfMayhem.decreaseSuccessLatency()
            case Character(c) if c == 't' =>
              sourceOfMayhem.decreaseRequestTimeout()
            case Character(c) if c == 'a' && s.isStarted =>
              for {
                updatedState <- state.updateAndGet(s => s.copy(
                  circuitBreakerConfiguration = s.circuitBreakerConfiguration.copy(
                    maxFailures = {
                      val max = s.circuitBreakerConfiguration.maxFailures / 2
                      if (max >= 1) {
                        max
                      } else {
                        1
                      }
                    }
                  )
                ))
                demoProgram <- createDemoProgram(updatedState.circuitBreakerConfiguration)
                _ <- state.modify(s => (s.copy(
                  demoProgram = Option(demoProgram)
                ), s))
                _ <- updateProgramExecutor()
              } yield ()
            case Character(c) if c == 'r' =>
              ???
            case Character(c) if c == 'm' =>
              ???
            case _ =>
              Monad[F].unit
          }
        } yield ()

      case _ =>
        Monad[F].unit
    }
  } yield true

  private def updateProgramExecutor() = {
    for {
      s <- state.get
      _ <- s.demoProgramExecutor.traverse(_.cancel)
      demoProgramExecutor <- forever(s.demoConfiguration.delayBetweenCallToSourceOfMayhemInNanos.nanos) {
        s.demoProgram.traverse(_.run()).start
      }.start
      _ <- state.modify(s => (s.copy(
        demoProgramExecutor = Option(demoProgramExecutor)
      ), s))
    } yield ()
  }

  private def forever(delay: FiniteDuration)(effect: => F[_]): F[Unit] =
    Temporal[F].sleep(delay) >> effect >> forever(delay)(effect)

  private def animate(frame: Int = 0, animation: List[(StatisticsInfo, Option[Input], Boolean, MayhemState, CircuitBreakerConfiguration) => String]): F[Unit] = {
    for {
      s <- state.get
      mayhemState <- sourceOfMayhem.mayhemState
      _ <- console.writeString(animation(frame)(
        s.statisticsInfo,
        s.previousInput,
        s.isStarted,
        mayhemState,
        s.circuitBreakerConfiguration
      )) >>
        Temporal[F].sleep(500.milli) >>
        console.clear() >>
        animate(if (frame < animation.size - 1) frame + 1 else 0, animation)
    } yield ()
  }

  override def exit(): F[Unit] = for {
    s <- state.get
    _ <- s.currentAnimation.traverse(_.cancel)
    _ <- s.demoProgramExecutor.traverse(_.cancel)
    _ <- s.statisticsPoller.traverse(_.cancel)
  } yield ()

}
