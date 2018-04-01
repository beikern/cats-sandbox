package com.github.beikern.monads

import cats.data.State
import cats.syntax.applicative._ // for pure
object StateMonadApp extends App {

  type CalcState[A] = State[List[Int], A]

  def opState(f: (Int, Int) => Int): CalcState[Int] =
    for {
      _ <- State.modify[List[Int]](s => {
        val (op, state) = s.splitAt(2)
        state.+:(op.reduceLeft(f))
      })
      ans <- State.get[List[Int]]
    } yield {
      ans.head
    }

  def valueState(v: Int): CalcState[Int] =
    for {
      _ <- State.modify[List[Int]](_.+:(v))
    } yield v

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+"    => opState(_ + _)
      case "-"    => opState(_ - _)
      case "*"    => opState(_ * _)
      case "/"    => opState(_ / _)
      case number => valueState(Integer.parseInt(number))
    }
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState])(
      (state, sym) =>
        for {
          _ <- state
          ans <- evalOne(sym)
        } yield ans
    )
  }

  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    _ <- evalOne("+")
    _ <- evalOne("3")
    _ <- evalOne("4")
    _ <- evalOne("+")
    ans <- evalOne("*")
  } yield ans

  val program3 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(s"program3 result = ${program3.run(Nil).value}")
  println("evalInput result = " + evalInput("1 2 + 3 4 + *"))

}
