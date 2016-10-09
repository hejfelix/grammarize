import cats.free.Free
import cats.free.Free.liftF
import cats.{~>, data, Eval}
import cats.data.{State, StateT}

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

sealed trait GrammarA[A]
case class Regx[T](key: Regex)                                   extends GrammarA[T]
case class Optional[T](grammar: Free[GrammarA, T])               extends GrammarA[T]
case class Choice[T](g: Free[GrammarA, T], f: Free[GrammarA, T]) extends GrammarA[T]
case class Multi[T](g: Free[GrammarA, T])                        extends GrammarA[T]

object FreeAppGrammar extends App {

  type Grammar[A] = Free[GrammarA, A]

  def regx(regex: Regex): Grammar[String] =
    liftF(Regx[String](regex))

  def choice[T](g: Grammar[T], f: Grammar[T]): Grammar[T] =
    liftF(Choice(g, f))

  def optional[T](g: Grammar[T]): Grammar[T] =
    liftF(Optional[T](g))

  def multi[T](g: Grammar[T]): Grammar[T] =
    liftF(Multi[T](g))

  implicit class GrammarOps[T](g: Grammar[T]) {
    def ~(h: Grammar[T]): Grammar[T] = for (a <- g; b <- h) yield b
    def ? : Grammar[T]               = optional(g)
    def |(h: Grammar[T]): Grammar[T] = choice(g, h)
    def + : Grammar[T]               = multi(g)
  }

  def grammar: Grammar[String] = {
    val aOrB: Grammar[String]   = regx("a".r) | regx("b".r)
    val one: Grammar[String]    = regx("1".r)
    val maybe2: Grammar[String] = regx("2".r).?
    val three: Grammar[String]  = regx("3".r)
    one ~ maybe2 ~ three ~ aOrB
  }

  def listGrammar = {
    val openBracket = regx("\\[".r)
    val digit       = regx("\\d".r)
    val endBracket  = regx("\\]".r)
    val moreDigits  = (regx(",".r) ~ digit).+
    openBracket ~ digit ~ (moreDigits).? ~ endBracket
  }

  val testStrings = Seq(
    "123",
    "13a",
    "123b",
    "12b",
    "133a",
    "1",
    "123a1337",
    "13b42"
  )

  parseAndPresent(regx("7".r).+)("7778")
  parseAndPresent(listGrammar)("[5,4,10]")

  def parseAndPresent(g: Grammar[String])(s: String) = {
    val parseResult = g.foldMap(parserInterpreter).run(s).value
    println(s"$s:  $parseResult")
  }

  testStrings map parseAndPresent(grammar)

  type ParserInterpreterState[A] = State[String, A]

  def parseOptional[A](runB: ParserInterpreterState[A]): State[String, A] =
    State.apply(state => {
      runB
        .map(_ match {
          case ParseFailure() => ParseSuccess(state).asInstanceOf[A]
          case p @ _          => p.asInstanceOf[A]
        })
        .run(state)
        .value
    })

  def parseRegex[A](regex: Regex): State[String, A] =
    State.apply(state =>
      regex.findPrefixMatchOf(state) match {
        case Some(Match(str)) =>
          val rest = state.drop(str.length)
          (rest, ParseSuccess(rest).asInstanceOf[A])
        case None =>
          (state, ParseFailure().asInstanceOf[A])

    })

  def parserInterpreter: GrammarA ~> ParserInterpreterState =
    new (GrammarA ~> ParserInterpreterState) {
      def apply[A](fa: GrammarA[A]): ParserInterpreterState[A] =
        fa match {
          case Regx(regexp) => parseRegex(regexp)
          case Optional(b)  => parseOptional(b.foldMap(this))
          case m @ Multi(g) =>
            def x: State[String, A] =
              State.apply(state => {
                g.foldMap(this)
                  .run(state)
                  .map {
                    case (s, ParseSuccess(_))    => x.run(s).value
                    case r @ (s, ParseFailure()) => (s, ParseSuccess(s).asInstanceOf[A])
                  }
                  .value
              })
            x
          case Choice(a, b) =>
            State.apply(state => {
              val runA = a.foldMap(this).run(state).value
              if (runA._2.asInstanceOf[ParseResult[_]].isSuccess)
                runA
              else {
                b.foldMap(this).run(state).value
              }
            })
        }
    }

}
