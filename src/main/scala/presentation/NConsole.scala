package com.github.morotsman
package presentation

import cats.effect.Sync
import cats.Monad
import com.github.morotsman.presentation.SpecialKey.{SpecialKey, Unknown, Up}
import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability

sealed trait Input

final case class Key(k: SpecialKey) extends Input

final case class Character(c: Char) extends Input


object SpecialKey extends Enumeration {
  type SpecialKey = Value
  val Up, Down, Left, Right, Esc, Unknown = Value
}

trait NConsole[F[_]] {
  def read(): F[Input]

  def writeCharacter(c: Char): F[Unit]

  def writeString(s: String): F[Unit]

  def clear(): F[Unit]
}

object NConsole {
  private val terminal = TerminalBuilder.terminal()
  terminal.enterRawMode()
  private val reader = terminal.reader()

  def make[F[_] : Sync]()(implicit M: Monad[F]): F[NConsole[F]] = {
    Sync[F].delay(
      new NConsole[F] {
        override def read(): F[Input] = M.pure {
          var input = reader.read().toChar
          if (input == 27) {
            input = reader.read().toChar
            if (input == '[') {
              input = reader.read().toChar
              input match {
                case 'A' => Key(SpecialKey.Up)
                case 'D' => Key(SpecialKey.Left)
                case 'C' => Key(SpecialKey.Right)
                case 'B' => Key(SpecialKey.Down)
                case _ => Key(SpecialKey.Unknown)
              }
            } else {
              Key(SpecialKey.Esc)
            }
          } else {
            Character(input)
          }
        }

        override def writeCharacter(c: Char): F[Unit] = M.pure {
          println(c)
        }

        override def writeString(s: String): F[Unit] = M.pure {
          println(s)
        }

        override def clear(): F[Unit] = M.pure {
          terminal.puts(Capability.clear_screen)
          terminal.flush
        }
      })
  }
}
