package com.github.beikern.monoids

trait SemigroupCustom[A] {
  def combine(x: A, y: A): A
}

trait MonoidCustom[A] extends SemigroupCustom[A] {
  def empty: A
}

object MonoidCustom {
  def apply[A](implicit monoid: MonoidCustom[A]) = monoid
}
