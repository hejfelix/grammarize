import cats.{Monad, RecursiveTailRecM}

import scala.annotation.tailrec

object ParseResult {
  implicit def monad[T] =
    new Monad[ParseResult] with RecursiveTailRecM[ParseResult] {
      override def pure[A](x: A): ParseResult[A] = ParseSuccess(x)
      override def flatMap[A, B](fa: ParseResult[A])(
          f: (A) => ParseResult[B]): ParseResult[B] =
        fa match {
          case pf @ ParseFailure() => ParseFailure()
          case ParseSuccess(remaining) => f(remaining)
        }
      @tailrec
      override def tailRecM[A, B](a: A)(
          f: (A) => ParseResult[Either[A, B]]): ParseResult[B] =
        f(a) match {
          case ParseSuccess(Right(result)) => ParseSuccess(result)
          case ParseSuccess(Left(result)) => tailRecM(result)(f)
          case ParseFailure() => ParseFailure()
        }
    }
}

sealed trait ParseResult[+T] {
  def isSuccess: Boolean = false
}

case class ParseSuccess[T](remaining: T) extends ParseResult[T] {
  override def isSuccess: Boolean = true
}
case class ParseFailure() extends ParseResult[Nothing]
