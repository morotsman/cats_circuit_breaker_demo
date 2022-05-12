package com.github.morotsman
package presentation

import cats.effect.Sync

trait Slide[F[_]] {
  def show(): F[Unit]
  def userInput(input: Input): F[Unit]
}