package com.github.beikern.monads

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._ // for tell
import cats.instances.vector._ // for Monoid

object WriterApp extends App {
  type Logged[A] = Writer[Vector[String], A]

  42.pure[Logged].map(_ + 1)

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)
  def factorial(n: Int): Logged[Int]= {
    for {
      ans <-  if(n == 0) {
                1.pure[Logged]
              } else {
                slowly(factorial(n - 1).map(_ * n))
              }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  def factorial2(n: Int): Logged[Int]= {
    if(n == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial(n - 1).map(_ * n))
    }.flatMap(ans => Vector(s"fact $n $ans").tell.map(_ => ans))

  }


  val result = factorial(5).run
  println(s"vectorLog -> ${result._1.mkString(", ")} result -> ${result._2}")
}
