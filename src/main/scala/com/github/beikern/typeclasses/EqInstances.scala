package com.github.beikern.typeclasses

import cats.kernel.Eq

trait EqInstances {
  implicit val catEqInstance: Eq[Cat] = Eq.instance[Cat]((c1, c2) => c1.name == c2.name && c1.color == c2.color && c1.age == c2.age)
}

object EqInstances extends EqInstances