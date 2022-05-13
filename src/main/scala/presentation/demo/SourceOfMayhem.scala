package com.github.morotsman
package presentation.demo

import cats._
import cats.implicits._
import cats.{FlatMap, MonadError}
import cats.effect.{Ref, Temporal}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait SourceOfMayhem[F[_]] {
  def mightFail(): F[Unit]

  def toggleFailure(): F[Unit]

  def setSuccessLatency(duration: FiniteDuration): F[Unit]

  def setRequestTimeout(duration: FiniteDuration): F[Unit]
}

final case class MayhemState(
                              isFailing: Boolean,
                              successLatency: FiniteDuration,
                              requestTimeout: FiniteDuration
                            )

object SourceOfMayhem {

  def make[F[_] : Temporal : MonadError[*[_], Throwable]](state: Ref[F, MayhemState]): SourceOfMayhem[F] =
    new SourceOfMayhem[F] {
      override def mightFail(): F[Unit] = for {
        currentState <- state.get
        _ <- if (!currentState.isFailing) {
          Temporal[F].sleep(currentState.successLatency) >> MonadError[F, Throwable].unit
        } else {
          Temporal[F].sleep(currentState.requestTimeout) >> MonadError[F, Throwable].raiseError(new RuntimeException("Boooom"))
        }
      } yield ()

      override def toggleFailure(): F[Unit] =
        state.modify(s => (s.copy(isFailing = !s.isFailing), s))

      override def setSuccessLatency(successLatency: FiniteDuration): F[Unit] =
        state.modify(s => (s.copy(successLatency = successLatency), s))

      override def setRequestTimeout(requestTimeout: FiniteDuration): F[Unit] =
        state.modify(s => (s.copy(requestTimeout = requestTimeout), s))
    }

}
