import cats.free.Free
import cats.free.Free.liftF
import cats.{Monad, RecursiveTailRecM, ~>}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

sealed trait GrammarA[A]
case class Regx[T](key: Regex) extends GrammarA[T]
case class Optional[T](grammar: Free[GrammarA, T]) extends GrammarA[T]

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
sealed trait ParseResult[+T]
case class ParseSuccess[T](remaining: T) extends ParseResult[T]
case class ParseFailure() extends ParseResult[Nothing]

object FreeAppGrammar extends App {

  type Grammar[A] = Free[GrammarA, A]

  // Put returns nothing (i.e. Unit).
  def regx[T](key: Regex): Grammar[T] =
  liftF[GrammarA, T](Regx[T](key))

  // Get returns a T value.
  def optional[T](g: Grammar[T]): Grammar[T] =
  liftF[GrammarA, T](Optional[T](g))

  implicit class FollowedBy[T](g: Grammar[T]) {
    def ~(h: Grammar[T]) = for (a <- g; b <- h) yield b
  }

  def program: Free[GrammarA, String] =
    regx[String]("1".r) ~ optional(regx("2".r)) ~ regx("3".r)

  println(program)
  private val result: String =
    program.foldMap(impureParserInterpreter("13479")) match {
      case ParseSuccess(remaining) => remaining
      case ParseFailure() =>  ""
    }
  println(result)

  def impureParserInterpreter(input: String): GrammarA ~> ParseResult =
    new (GrammarA ~> ParseResult) {
      var state = input
      def apply[A](fa: GrammarA[A]): ParseResult[A] =
        fa match {
          case Regx(key) =>
            val result: ParseResult[String] =
              key.findPrefixMatchOf(state) match {
                case Some(Match(str)) =>
                  state = state.drop(str.length)
                  ParseSuccess(state)
                case None =>
                  ParseFailure()
              }
            result.asInstanceOf[ParseResult[A]]
          case Optional(b) =>
            val result: ParseResult[Any] = b.foldMap(this) match {
              case ParseSuccess(remaining) => ParseSuccess(remaining)
              case ParseFailure() => ParseSuccess(state)
            }
            result.asInstanceOf[ParseResult[A]]
        }
    }

}
