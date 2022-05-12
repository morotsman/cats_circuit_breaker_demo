package com.github.morotsman
package presentation.tools

import cats.effect.Fiber

trait Slide[F[_]] {
  def show(): F[Fiber[F, Throwable, Unit]]

  def userInput(input: Input): F[Unit]
}
