package com.github.beikern.usescases.validation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import cats.syntax.semigroup._
import cats.syntax.apply._

sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
  def apply(value: A)(implicit ev: Semigroup[E]): Validated[E, A] = {
    this match {
      case Pure(f) => f(value)
      case And(left, right) =>
        (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) =>
        left(value) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) =>
            right(value) match {
              case Valid(a) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
  }
}

case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]
case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]


object ValidationApp extends App {

  type ValidatedInt = Validated[List[String], Int]

  import cats.instances.list._

  val mustBeGreaterThanTen =
    Pure {
      val f: Int => ValidatedInt =
        a =>
          if (a > 10) a.valid
          else List("value must be greater than 10").invalid
      f
    }

  val mustBeGreaterThanTwelve =
    Pure {
      val f: Int => ValidatedInt =
        a =>
          if (a > 12) a.valid
          else List("value must be greater than 12").invalid
      f
    }

  val mustBeEqual1 =
    Pure {
      val f: Int => ValidatedInt =
        a =>
          if (a == 1) a.valid else List("value should be 1").invalid
      f
    }

  val gtThan10And12 = mustBeGreaterThanTen and mustBeGreaterThanTwelve
  val gtThan10And12Oris1 = gtThan10And12 or mustBeEqual1

  println(gtThan10And12Oris1(1))



}
