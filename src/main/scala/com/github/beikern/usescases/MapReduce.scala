package com.github.beikern.usescases

import cats.Traverse
import cats.kernel.Monoid
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.foldable._
import cats.instances.future._
import cats.instances.list._

import scala.concurrent.{Await, Future}

trait MapReduce {
  def foldMap[A, B: Monoid](s: Vector[A])(f: A => B): B = {
    s.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
  }

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores
    = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    val batches: Iterator[Vector[A]] = values.grouped(groupSize)
    Future.sequence(batches.map(v => Future {
      foldMap(v)(func)
    })).map(_.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  def parallelFoldMapWithMoreCats[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores
    = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    val batches: List[Vector[A]] = values.grouped(groupSize).toList
    val futuresFold: List[Future[B]] = batches.map(v => Future {
      foldMap(v)(func)
    })

    Traverse[List].sequence(futuresFold).map(_.combineAll)
  }
}

object MapReduceApp extends MapReduce with App{
  import cats.instances.int._ // for Monoid
  import scala.concurrent.duration._

  println(foldMap(Vector(1,2,3))(identity))
  println(Runtime.getRuntime.availableProcessors)
  System.nanoTime()
  val st1 = System.nanoTime
  foldMap((1 to 10000000).toVector)(identity)
  val et1 = System.nanoTime - st1
  println(et1)
  val st2 = System.nanoTime
  Await.result(parallelFoldMap((1 to 10000000).toVector)(identity), 10.seconds)
  val et2 = System.nanoTime - st2
  val st3 = System.nanoTime
  Await.result(parallelFoldMapWithMoreCats((1 to 10000000).toVector)(identity), 10.seconds)
  val et3 = System.nanoTime - st3
  println(et3)
}