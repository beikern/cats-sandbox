package com.github.beikern.monads

import scala.language.higherKinds

trait MonadCustom[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] = {
    flatMap[A, B](value)(a => pure(func(a)))
  }
}

