package com.github.beikern.functors

final case class Box[A](value: A)

trait PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }
  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  implicit def printableBox[A](implicit printable: Printable[A]): Printable[Box[A]] = {
    printable.contramap[Box[A]](a => a.value)
  }
}

object  printableInstances extends PrintableInstances