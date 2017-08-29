package task

import scala.io.Source
import scala.util.{Failure, Try}
import java.io.FileNotFoundException
import java.io.File
import java.io.PrintWriter
import scala.annotation.tailrec

trait ResourceReader {
  def readResource(filename: String): Either[Throwable, List[String]] =
    Try(Source.fromResource(filename).getLines.toList).recoverWith { case _ =>
      Failure(new FileNotFoundException(filename))
    }.toEither

  def clientsEither(filename: String = "clients.txt"): Either[Throwable, ClientListContainer] = {
    for {
      lines <- readResource(filename)
      clientList <- Try {
        val list = lines.map { line =>
          line.split("\t").toList match {
            case List(index, balance, securityA, securityB, securityC, securityD) =>
              Client(index, balance.toInt, securityA.toInt, securityB.toInt, securityC.toInt, securityD.toInt)
            case _ => sys.error(s"Invalid file format: $filename")
          }
        }

        if (list.size > list.groupBy(_.index).size) sys.error("Invalid Clients format")
        else list
      }.toEither
    } yield ClientListContainer(clientList)
  }

  def ordersEither(filename: String = "orders.txt"): Either[Throwable, OrderListContainer] = {
    for {
      lines <- readResource(filename)
      orderList <- Try {
        lines.map { line =>
          line.split("\t").toList match {
            case List(client, "b", symbol, price, qty) => BuyOrder(client, Security(symbol), price.toInt, qty.toInt)
            case List(client, "s", symbol, price, qty) => SellOrder(client, Security(symbol), price.toInt, qty.toInt)
            case _ => sys.error(s"Invalid file format: $filename")
          }
        }
      }.toEither
    } yield OrderListContainer(orderList)
  }
}

object Main extends ResourceReader {
  def run(clientContainer: ClientListContainer,
          orderContainer: OrderListContainer): Either[Throwable, ClientListContainer] = {
    @tailrec
    def go(buyOrders: List[Order], sellOrders: List[Order], result: ClientListContainer): ClientListContainer = {
      buyOrders match {
        case Nil => result
        case head :: tail =>
          sellOrders.filter(_.client != head.client).find(ord => ord.price == head.price && ord.qty == head.qty) match {
            case Some(found) =>
              val newResult = result.update(head).update(found)
              go(tail, sellOrders.filter(_ != found), newResult)
            case None => go(tail, sellOrders, result)
          }
      }
    }

    Try(go(orderContainer.list(), orderContainer.list(false), clientContainer)).toEither
  }

  def main(args: Array[String]): Unit = {
    val program = for {
      clients <- clientsEither()
      orders <- ordersEither()
      result <- run(clients, orders)
      file <- result.writeToFile
    } yield file

    program match {
      case Right(file) => println(s"Program has been completed successfully! Check the output file ${file.getName}!")
      case Left(ex) => println(s"Program has been finished with error! $ex")
    }
  }
}

case class Property(security: Security, qty: Int)
case class Client(index: String, balance: Int, securities: List[Property]) {
  def update(order: Order): Client = {
    val (newBalance, newSecurities) = order match {
      case buy: BuyOrder =>
        val newBalance = balance - buy.totalPrice
        val newSecurities =
          securities.find(_.security == buy.security) match {
            case Some(found) => securities.filter(_.security != found.security) :+ found.copy(qty = found.qty + buy.qty)
            case None => securities :+ Property(buy.security, buy.qty)
          }

        (newBalance, newSecurities)
      case sell: SellOrder =>
        val newBalance = balance + sell.totalPrice
        val newSecurities =
          securities.find(_.security == sell.security) match {
            case Some(found) => securities.filter(_.security != found.security) :+ found.copy(qty = found.qty - sell.qty)
            case None => securities :+ Property(sell.security, sell.qty)
          }

        (newBalance, newSecurities)
    }

    Client(index, newBalance, newSecurities)
  }
}

object Client {
  def apply(index: String, balance: Int, qtyA: Int, qtyB: Int, qtyC: Int, qtyD: Int): Client =
    Client(index, balance, List(Property(A, qtyA), Property(B, qtyB), Property(C, qtyC), Property(D, qtyD)))
}

case class OrderListContainer(data: List[Order]) {
  def list(buy: Boolean = true): List[Order] = data.foldLeft(List[Order]()) { (res, order) =>
    order match {
      case buyOrder: BuyOrder if buy => res :+ buyOrder
      case sellOrder: SellOrder if !buy => res :+ sellOrder
      case _ => res
    }
  }
}

case class ClientListContainer(data: List[Client]) {
  def update(order: Order): ClientListContainer =
    data.find(_.index == order.client) match {
      case Some(found) => ClientListContainer(data.filter(_ != found) :+ found.update(order))
      case None => this
    }

  def writeToFile: Either[Throwable, File] = Try {
    val file = new File("result.txt")
    val writer = new PrintWriter(file)
    val text = data.sortBy(_.index).map { line =>
      val securities =
        line.securities.sortWith((s1, s2) => s1.security.toString > s2.security.toString).map(_.qty).mkString("\t")
      s"${line.index}\t${line.balance}\t$securities"
    }.mkString("\n")

    writer.write(text)
    writer.close()
    file
  }.toEither
}

sealed trait Order {
  val client: String
  val price: Int
  val qty: Int
  def totalPrice = price * qty
}
case class BuyOrder(client: String, security: Security, price: Int, qty: Int) extends Order
case class SellOrder(client: String, security: Security, price: Int, qty: Int) extends Order

sealed trait Security
object Security {
  def apply(symbol: String): Security = symbol match {
    case "A" => A
    case "B" => B
    case "C" => C
    case "D" => D
    case _ => sys.error("Invalid Security symbol")
  }
}

case object A extends Security
case object B extends Security
case object C extends Security
case object D extends Security