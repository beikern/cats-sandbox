package com.github.beikern.monads

import cats.data.EitherT

import scala.concurrent.{Await, Future}
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

object MonadTransformers extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"
      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"
      -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(powerLevel) => powerLevel.pure[Response]
      case None             => s"$autobot unreachable".raiseError[Response, Int]
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      pl1 <- getPowerLevel(ally1)
      pl2 <- getPowerLevel(ally2)
    } yield {
      (pl1 + pl2) > 15
    }
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    import scala.concurrent.duration._

    val result: Future[Either[String, String]] = canSpecialMove(ally1, ally2)
      .map(
        b =>
          if (b) s"$ally1 and $ally2 are ready to roll out!"
          else s"$ally1 or $ally2 needs to recharge!")
      .value
    Await.result(result, 10.seconds).fold(identity, identity)
  }

  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
