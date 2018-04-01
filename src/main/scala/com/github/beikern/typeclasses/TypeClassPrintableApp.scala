package com.github.beikern.typeclasses

object TypeClassPrintableApp extends App {
  import PrintableInstances._

  val orionCat = Cat("Orión", 1, "skinny")
  val onyxCat = Cat("Onyx", 1, "grey")

  Printable.print(orionCat)
  Printable.print(onyxCat)

  import PrintableSyntax._
  orionCat.print
  onyxCat.print
}