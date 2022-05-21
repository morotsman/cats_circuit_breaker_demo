package com.github.morotsman
package presentation.tools

import cats.implicits._
import cats.FlatMap


object DebugHelper {
  /** Extension methods for an effect of type `F[A]`. */ implicit class DebugHelper[F[_] : FlatMap, A](fa: F[A]) {
    /** Print to the console the value of the effect
     * along with the thread it was computed on. */
    def debug: F[A] =
      for {
        a <- fa
        tn = Thread.currentThread.getName
        _ = println(s"[$tn}] $a")
      } yield a }
}
