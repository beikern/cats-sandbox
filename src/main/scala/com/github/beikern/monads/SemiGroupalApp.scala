package com.github.beikern.monads

object SemiGroupalApp {
  import cats.implicits._

  import cats.Monad
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    x.flatMap(a => y.map(b => (a,b)))
  }
}
