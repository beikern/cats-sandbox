package com.github.beikern.monoids

trait CustomMonoidInstances {
  val AndInstance = new MonoidCustom[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
  val OrInstance = new MonoidCustom[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
  val xorInstance = new MonoidCustom[Boolean] {
    def combine(a: Boolean, b: Boolean): Boolean =
      (a && !b) || (!a && b)
    def empty: Boolean = false
  }
  val xNorInstance = new MonoidCustom[Boolean] {
    def combine(a: Boolean, b: Boolean) =
      (!a || b) && (a || !b)
    def empty = true
  }

  def setUnionInstance[A] = new MonoidCustom[Set[A]] {
    override def empty: Set[A] = Set.empty
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }
}

