package com.github.beikern.functors

import cats.Functor
import cats.syntax.functor._
import cats.instances.option._
import cats.instances.list._
import functorInstances._
import printableInstances._

object FunctorApp extends App {
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = {
    start.map(n => n + 1 * 2)
  }

  println(doMath(List(1,2,3,4)))
  println(doMath(Option(1)))

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  val f = (v: Int) => v.toString+"wololo"
  val f2 = (v: String) => v+"haha!"
  tree.map(f)
  println(tree)
  println(tree.map(f).map(f2))

  val x: Box[Boolean] = Box(true)

  println(s"printable box: ${Printable.format(x)}")

}
