import cats.arrow.FunctionK
import cats.data.State
import cats.free.Free
import cats.free.Free.liftF
import com.mifmif.common.regex.Generex

import scala.util.Random
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
    def ~(h: Grammar[T]): Grammar[T] = for (_ <- g; b <- h) yield b
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

  def listGrammar: Grammar[String] = {
    val openBracket = regx("\\[".r)
    val digit       = regx("\\d".r)
    val endBracket  = regx("\\]".r)
    val moreDigits  = (regx(",".r) ~ digit).+
    openBracket ~ digit ~ moreDigits.? ~ endBracket
  }

  val generator                        = generate(sizeHint = 42, randomSeed = 1337)
  val generated: GenerateState[String] = listGrammar.foldMap(generator)

  println(listGrammar)
  for (_ <- 0 to 20) {
    println(generated.run(initial = "").value._2)
  }
  type ParserInterpreterState[A] = State[String, A]

  def parseOptional[A](runB: ParserInterpreterState[A]): State[String, A] =
    State.apply(state => {
      runB
        .map {
          case ParseFailure() => ParseSuccess(state).asInstanceOf[A]
          case p @ _          => p.asInstanceOf[A]
        }
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

  type GenerateState[R] = State[String, R]

  def generate(sizeHint: Int, randomSeed: Long = System.currentTimeMillis()): FunctionK[GrammarA, GenerateState] =
    new FunctionK[GrammarA, GenerateState] {

      private val javaRandom  = new java.util.Random(randomSeed)
      private val scalaRandom = new Random(randomSeed)

      def multi(g: Grammar[_], count: Int): GenerateState[String] =
        for {
          _    <- g.foldMap(this)
          rest <- if (count == 0) State((s: String) => (s, s)) else multi(g, count - 1)
          _    <- State.set(rest.asInstanceOf[String])
        } yield {
          rest.asInstanceOf[String]
        }

      override def apply[A](fa: GrammarA[A]): GenerateState[A] = fa match {
        case Regx(reg) =>
          State { str =>
            val generex   = new Generex(reg.pattern.pattern(), javaRandom)
            val generated = generex.random()
            val res       = str + generated
            (res, res.asInstanceOf[A])
          }
        case Optional(grammar) =>
          val doGenerate = scalaRandom.nextBoolean()
          if (doGenerate) {
            grammar.foldMap(this)
          } else {
            State(str => (str, str.asInstanceOf[A]))
          }
        case Choice(g, f) =>
          val pattern: Free[GrammarA, A] = if (scalaRandom.nextBoolean()) g else f
          pattern.foldMap(this)
        case Multi(g) => multi(g, scalaRandom.nextInt(sizeHint)).map(_.asInstanceOf[A])
      }
    }

  def parserInterpreter: FunctionK[GrammarA, ParserInterpreterState] =
    new FunctionK[GrammarA, ParserInterpreterState] {
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
                    case (s, ParseSuccess(_)) => x.run(s).value
                    case (s, ParseFailure())  => (s, ParseSuccess(s).asInstanceOf[A])
                    case (_, _)               => ???
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
