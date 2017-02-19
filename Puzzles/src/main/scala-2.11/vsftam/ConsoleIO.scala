package vsftam

/**
  * Created by vincenttam on 2/18/17.
  */

import scala.io.StdIn.readLine

object ConsoleIO {

  sealed trait ConsoleIO[A] {
    def map[B](f: A => B): ConsoleIO[B] = Map(this, f)
    def flatMap[B](f: A => ConsoleIO[B]): ConsoleIO[B] = Chain(this, f)
  }
  final case class ReadLine() extends ConsoleIO[String]
  final case class WriteLine(line: String) extends ConsoleIO[Unit]
  final case class Pure[A](value: A) extends ConsoleIO[A]
  final case class Map[A, B](v: ConsoleIO[A], f: A => B) extends ConsoleIO[B]
  final case class Chain[A, B](v: ConsoleIO[A], f: A => ConsoleIO[B]) extends ConsoleIO[B]

  def interpret[A](program: ConsoleIO[A]) : A = program match {
    case ReadLine() => readLine()
    case WriteLine(l) => println(l); ()
    case Pure(v) => v
    case Map(v, f) => f(interpret(v))
    case Chain(v, f) => interpret(f(interpret(v)))
  }

  def main(args: Array[String]): Unit = {

    def conversation = Chain[Unit, String](
      WriteLine("What is your name?"),
      _ => Chain[String, String](
        ReadLine(), name =>
          Chain[Unit, String](WriteLine(s"My name is $name"),
            _ => Pure[String](name))
      )
    )
    interpret(conversation)
  }
}
