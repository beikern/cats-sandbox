package com.github.beikern.typeclasses
import cats.syntax.show._
import ShowImplicits._

object TypeClassShowApp extends App{
  val orionCat = Cat("Orión", 1, "skinny")
  val onyxCat = Cat("Onyx", 1, "grey")

  println(s"using cats show: ${orionCat.show}")
  println(s"using cats show: ${onyxCat.show}")
  orionCat.print
  onyxCat.show
}
