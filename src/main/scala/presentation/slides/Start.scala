package com.github.morotsman
package presentation.slides

import cats.effect.implicits._
import cats.effect.{Fiber, Spawn, Sync}
import com.github.morotsman.presentation.tools.{Input, NConsole, Slide}

case class Start[F[_] : Sync : Spawn](console: NConsole[F]) extends Slide[F] {
  override def show(): F[Fiber[F, Throwable, Unit]] = {
    val text =
      """
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |                     _______ _________ _______  _______          __________________   ______   _______  _______  _______  _        _______  _______
        |                    (  ____ \\__   __/(  ____ )(  ____ \|\     /|\__   __/\__   __/  (  ___ \ (  ____ )(  ____ \(  ___  )| \    /\(  ____ \(  ____ )
        |                    | (    \/   ) (   | (    )|| (    \/| )   ( |   ) (      ) (     | (   ) )| (    )|| (    \/| (   ) ||  \  / /| (    \/| (    )|
        |                    | |         | |   | (____)|| |      | |   | |   | |      | |     | (__/ / | (____)|| (__    | (___) ||  (_/ / | (__    | (____)|
        |                    | |         | |   |     __)| |      | |   | |   | |      | |     |  __ (  |     __)|  __)   |  ___  ||   _ (  |  __)   |     __)
        |                    | |         | |   | (\ (   | |      | |   | |   | |      | |     | (  \ \ | (\ (   | (      | (   ) ||  ( \ \ | (      | (\ (
        |                    | (____/\___) (___| ) \ \__| (____/\| (___) |___) (___   | |     | )___) )| ) \ \__| (____/\| )   ( ||  /  \ \| (____/\| ) \ \__
        |                    (_______/\_______/|/   \__/(_______/(_______)\_______/   )_(     |/ \___/ |/   \__/(_______/|/     \||_/    \/(_______/|/   \__/
        |
        |
        |"""
    console.writeString(text.stripMargin).start
  }

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}

