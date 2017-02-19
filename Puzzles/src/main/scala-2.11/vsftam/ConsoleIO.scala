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

    def conversation: ConsoleIO[String] =
      Chain[Unit, String](
        WriteLine("What is your name?"),
        _ => Chain[String, String](
          ReadLine(),
          name => Chain[Unit, String](
            WriteLine(s"My name is $name"),
            _ => Pure[String](name))
        )
      )
    interpret(conversation)
  }
}

object FreeMonad {
  sealed trait Sequential[F[_], A] {
    def map[B](f: A => B): Sequential[F, B] = Map(this, f)
    def flatMap[B](f: A => Sequential[F, B]): Sequential[F, B] = Chain(this, f)
  }
  final case class Pure[F[_], A](value: A) extends Sequential[F, A]
  final case class Effect[F[_], A](fa: F[A]) extends Sequential[F, A]
  final case class Map[F[_], A, B](v: Sequential[F, A], f: A => B) extends Sequential[F, B]
  final case class Chain[F[_], A, B](v: Sequential[F, A], f: A => Sequential[F, B]) extends Sequential[F, B]

  trait ConsoleF[A]
  final case class ReadLine() extends ConsoleF[String]
  final case class WriteLine(line: String) extends ConsoleF[Unit]

  type ConsoleIO[A] = Sequential[ConsoleF, A]

  def interpret[A](program: ConsoleIO[A]) : A = program match {
    case Pure(v) => v
    case Effect(fv) => fv match {
      case ReadLine() => readLine()
      case WriteLine(line) => println(line); ()
    }
    case Map(v, f) => f(interpret(v))
    case Chain(v, f) => interpret(f(interpret(v)))
  }

  def main(args: Array[String]): Unit = {

    def userNameProgram: ConsoleIO[String] =
      Chain[ConsoleF, Unit, String](
        Effect[ConsoleF, Unit](WriteLine("What is your name")),
        _ => Chain[ConsoleF, String, String](
          Effect[ConsoleF, String](ReadLine()),
          name => Chain[ConsoleF, Unit, String](
            Effect[ConsoleF, Unit](WriteLine(s"Hello $name!")),
            _ => Pure[ConsoleF, String](name)
          )
        )
      )

    interpret(userNameProgram)
  }
}