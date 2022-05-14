package com.github.morotsman
package presentation.tools

trait Slide[F[_]] {
  def start(): F[Unit]

  def userInput(input: Input): F[Unit]

  def exit(): F[Unit]
}
