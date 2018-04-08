package com.github.beikern.usescases

import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.list._
import cats.instances.future._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Id}

import scala.concurrent.{Await, Future}
import scala.language.higherKinds // for traverse

trait UptimeClient[F[_]] {
  val hosts = Map("host1" -> 10, "host2" -> 6)
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {

  override def getUptime(hostname: String): Future[Int] = {
   Future.successful(hosts(hostname))
  }
}

trait TestUptimeClient extends UptimeClient[Id] {
  override def getUptime(hostname: String): Int = {
    hosts(hostname)
  }
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
   hostnames.traverse(client.getUptime).map(_.sum)
}

object testUpdateClient extends TestUptimeClient
object realUpdateClient extends RealUptimeClient

object UptimeClientApp extends App {
  import scala.concurrent.duration._


  println(Await.result(realUpdateClient.getUptime("host1"), 10.seconds))
  println(testUpdateClient.getUptime("host1"))
  val testUptimeClient = new TestUptimeClient{}

  println (new UptimeService(testUpdateClient).getTotalUptime(List("host1", "host2")))
  println (new UptimeService[Future](realUpdateClient).getTotalUptime(List("host1", "host2")))
}