package com.github.beikern.functors

import cats.Functor

trait FunctorInstances {
  implicit def TreeFunctorInstance = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l,r) => Branch(map(l)(f), map(r)(f))
      }
    }
  }
}

object functorInstances extends FunctorInstances