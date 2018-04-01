package com.github.beikern.functors

trait Printable[A] {
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        Printable.this.format(func(value))
    }
}
object Printable {
  def apply[A](implicit instance: Printable[A]) = instance
  def format[A](value: A)(implicit instance: Printable[A]): String = {
    instance.format(value)
  }
}
