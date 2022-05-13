package com.github.morotsman
package presentation.demo

import cats.implicits._
import cats.MonadError
import io.chrisdavenport.circuit.CircuitBreaker

case class DemoProgram[F[_] : MonadError[*[_], Throwable]](
                                                          sourceOfMayhem: SourceOfMayhem[F],
                                                          circuitBreaker: CircuitBreaker[F],
                                                          statistics: Statistics[F]
                                                          ) {
  def run(): F[Unit] = for {
    _ <- statistics.programCalled()
    _ <- circuitBreaker.protect {
      statistics.requestSent() >>
        sourceOfMayhem.mightFail().attemptTap(_ =>
          statistics.requestCompleted()
        )
    }.handleError(_ => ())
  } yield ()


}
