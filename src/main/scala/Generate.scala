import cats.arrow.FunctionK
import cats.data.State
import cats.implicits._

import scala.util.Random

class GenerateString[Token](val grammar: Grammar[Token]) {

  import grammar._
  type GenerateState[T] = State[List[Token], T]

  def apply(sizeHint: Int,
            randomSeed: Long = System.currentTimeMillis()): FunctionK[grammar.TokenGrammar, GenerateState] =
    new FunctionK[TokenGrammar, GenerateState] {

      private val scalaRandom = new Random(randomSeed)

      def multi(g: FreeGrammar[List[Token]], count: Int): GenerateState[List[Token]] = {
        val value: GenerateState[List[Token]] = g.foldMap(this)
        val nop                               = State[List[Token], List[Token]](s => (s, s))
        for {
          _    <- value
          rest <- if (count == 0) nop else multi(g, count - 1)
          _    <- State.set(rest)
        } yield rest
      }

      override def apply[A](fa: TokenGrammar[A]): GenerateState[A] = fa match {
        case BlackBox(_, generate) =>
          State { str =>
            val generated: List[Token] = generate(scalaRandom).map(_.asInstanceOf[Token])
            val res                    = str ++ generated
            (res, res.asInstanceOf[A])
          }
        case Optional(grammar) =>
          if (scalaRandom.nextBoolean()) {
            val value = grammar.foldMap(this)
            value
          } else {
            State(str => (str, str.asInstanceOf[A]))
          }
        case Choice(g, f) =>
          (if (scalaRandom.nextBoolean()) g else f).foldMap(this)
        case Multi(g) =>
          multi(g.map(_.asInstanceOf[List[Token]]), scalaRandom.nextInt(sizeHint)).map(_.asInstanceOf[A])
      }
    }

}
