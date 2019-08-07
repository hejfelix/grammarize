import FreeAppGrammar.{charGrammar, generator}
import cats.Id
import cats.arrow.FunctionK
import cats.free.Free
import cats.free.Free.liftF
import com.mifmif.common.regex.Generex

import scala.util.Random
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

class Grammar[Token] {

  sealed trait GrammarAlgebra[Serial, Parsed]
  type TokenGrammar[T] = GrammarAlgebra[Token, T]
  type FreeGrammar[T]  = Free[TokenGrammar, T]
  type GrammarT[T]     = Free[TokenGrammar, T]

  final case class Optional[T](grammar: Free[TokenGrammar, T])                   extends TokenGrammar[T]
  final case class Choice[T](g: Free[TokenGrammar, T], f: Free[TokenGrammar, T]) extends TokenGrammar[T]
  final case class Multi[T](g: Free[TokenGrammar, T])                            extends TokenGrammar[T]

  // Bring your own parser/generator
  final case class BlackBox[T](parse: List[Token] => ParseResult[List[T]], generate: Random => List[T])
      extends TokenGrammar[List[T]]

  object syntax {
    implicit class GrammarOps[T](g: FreeGrammar[T]) {
      def ~(h: FreeGrammar[T]): FreeGrammar[T] = for (_ <- g; b <- h) yield b
      def ? : FreeGrammar[T]                   = optional(g)
      def |(h: FreeGrammar[T]): FreeGrammar[T] = choice(g, h)
      def + : FreeGrammar[T]                   = multi(g)
    }
  }

  def regx(regex: Regex)(implicit ev: Token =:= Char): FreeGrammar[List[Char]] = {
    val value: TokenGrammar[List[Char]] = BlackBox[Char](
      tokens =>
        regex.findPrefixMatchOf(tokens.mkString) match {
          case None => ParseFailure(s"No match found for string ${tokens.mkString} using regular expression: $regex")
          case Some(Match(matchedString)) =>
            ParseSuccess(tokens.drop(matchedString.length).map(ev))
      },
      random =>
        new Generex(regex.pattern.pattern(), new java.util.Random(random.nextLong))
          .random()
          .toList
          .map(ev.flip.apply)
    )
    liftF(value)
  }

  def choice[T](g: FreeGrammar[T], f: FreeGrammar[T]): FreeGrammar[T] =
    liftF(Choice(g, f))

  def optional[T](g: FreeGrammar[T]): FreeGrammar[T] =
    liftF(Optional[T](g))

  def multi[T](g: FreeGrammar[T]): FreeGrammar[T] =
    liftF(Multi[T](g))
}

object FreeAppGrammar extends App {

  val charGrammar                     = new Grammar[Char]()
  val generator: GenerateString[Char] = new GenerateString(charGrammar)

  import generator.grammar._
  import generator.grammar.syntax._

  def grammar: FreeGrammar[List[Char]] = {
    val aOrB   = regx("a".r) | regx("b".r)
    val one    = regx("1".r)
    val maybe2 = regx("2".r).?
    val three  = regx("3".r)
    one ~ maybe2 ~ three ~ aOrB
  }

  def listGrammar: FreeGrammar[List[Char]] = {
    val openBracket  = regx("\\[".r)
    val digit        = regx("\\d".r)
    val closeBracket = regx("\\]".r)
    val moreDigits   = (regx(",".r) ~ digit).+
    openBracket ~ digit ~ moreDigits.? ~ closeBracket
  }

  val interpreter: FunctionK[generator.grammar.TokenGrammar, generator.GenerateState] =
    generator.apply(sizeHint = 42, randomSeed = 1337)
  val generated: generator.GenerateState[List[Char]] = listGrammar.foldMap(interpreter)

  println(listGrammar)
  for (_ <- 0 to 20) {
    println(generated.run(initial = Nil).value._2.mkString)
  }

}
