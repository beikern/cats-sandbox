package com.github.beikern.typeclasses

import EqInstances._
import cats.syntax.eq._
import cats.instances.option._
import cats.syntax.option._

object TypeClassEqInstance extends App{
  val orionCat = Cat("Orión", 1, "skinny")
  val onyxCat = Cat("Onyx", 1, "grey")

  val someOrion: Option[Cat] = Some(orionCat)

  println(someOrion === someOrion)
  println(orionCat.some === none[Cat])
  println(orionCat === onyxCat)
}
