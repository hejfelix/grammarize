import cats.{Id, ~>}
import cats.arrow.Choice
import cats.free.Free
import cats.free.Free.liftF
import cats.instances.string
import com.mifmif.common.regex.Generex

import scala.util.Random
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object DSL extends App {

  case class GrammarResult[T](i: String, result: T)

  sealed trait Grammar[A] {
    def ~[B](second: Grammar[B]): Grammar[A ~ B] = new ~(this, second)
    def |(second: Grammar[A]): Grammar[A]        = new |(this, second)
    def * : Grammar[A]                           = new *(this)
    def ? : Grammar[A]                           = new ?(this)
  }

  case class ?[T](g: Grammar[T])                            extends Grammar[T]
  case class *[T](g: Grammar[T])                            extends Grammar[T]
  case class Regx[T](r: Regex)                              extends Grammar[T]
  case class ~[T, U](first: Grammar[T], second: Grammar[U]) extends Grammar[T ~ U]
  case class |[T](first: Grammar[T], second: Grammar[T])    extends Grammar[T]

  val grammar: Grammar[~[~[String, String], String]] = Regx[String]("1".r) ~ Regx[String]("1".r) ~ Regx[String]("2".r)

  println(parse("11222", grammar))
  println(parse("01222", grammar))

  val oneOrTwo: Grammar[String] = Regx[String]("1".r) | Regx[String]("2".r)
  val g2                        = Regx[String]("1".r) ~ Regx[String]("0".r) ~ (oneOrTwo.*) ~ Regx[String]("q".r).?
  val rnd                       = new Random(1337)

  for (_ <- 0 to 100) {
    val gen = generate(g2)(rnd)
    println(parse(gen.result, g2), gen.result)
  }

  def generate[T](grammar: Grammar[T])(implicit rndGen: Random): GrammarResult[String] =
    grammar match {
      case Regx(r) =>
        val random: String = new Generex(r.regex).random()
        GrammarResult("", random)
      case *(grammar) =>
        val x = List.fill(rndGen.nextInt(5))(generate(grammar).result).mkString
        GrammarResult("", x)
      case ?(grammar) =>
        if (rndGen.nextBoolean())
          generate(grammar)
        else
          GrammarResult("", "")
      case ~(first, second) =>
        val fst: String = generate(first).result
        val snd: String = generate(second).result
        GrammarResult("", s"$fst$snd")
      case |(first, second) =>
        val fst: String = generate(first).result
        val snd: String = generate(second).result
        if (rndGen.nextBoolean)
          GrammarResult("", fst)
        else
          GrammarResult("", snd)
    }

  def parse[T](string: String, grammar: Grammar[T]): GrammarResult[Boolean] = grammar match {
    case Regx(r) =>
      r.findPrefixMatchOf(string) match {
        case Some(Match(str)) => GrammarResult(string.drop(str.length), true)
        case None             => GrammarResult(string, false)
      }
    case *(gr) =>
      val p = parse(string, gr)
      if (p.result == false) {
        GrammarResult(string, true)
      } else if (p.i == string || p.i == "") {
        p
      } else {
        parse(p.i, *(gr))
      }
    case ?(gr) =>
      val p = parse(string, gr)
      if (p.result == false) {
        GrammarResult(string, true)
      } else {
        p
      }
    case ~(first, second) =>
      val fst: GrammarResult[Boolean] = parse(string, first)
      parse(fst.i, second)
    case |(first, second) =>
      val fst: GrammarResult[Boolean] = parse(string, first)
      val snd: GrammarResult[Boolean] = parse(string, second)
      if (fst.result)
        fst
      else if (snd.result)
        snd
      else
        GrammarResult(string, false)
  }
//  import cats.arrow.FunctionK
//  import cats.{Id, ~>}
//  def impureCompiler: Grammar ~> Id =
//    new (Grammar ~> Id) {
//      def apply[A](g: Grammar[A]): Id[A] = g match {
//        case Processor(r)         => GrammarResult("", true)
//        case ~(gr, second)        => GrammarResult("", true)
//        case |(gr, first, second) => GrammarResult("", true)
//      }
//    }
//
//  program.foldMap(impureCompiler)

}
