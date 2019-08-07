import cats.arrow.FunctionK
import cats.data.State

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Parse {
//  type ParserInterpreterState[A] = State[String, A]
//
//  def parseOptional[A](runB: ParserInterpreterState[A]): State[String, A] =
//    State.apply(state => {
//      runB
//        .map {
//          case ParseFailure(err) => ParseSuccess(state).asInstanceOf[A]
//          case p @ _             => p.asInstanceOf[A]
//        }
//        .run(state)
//        .value
//    })
//
//  def parseRegex[A](regex: Regex): State[String, A] =
//    State.apply(state =>
//      regex.findPrefixMatchOf(state) match {
//        case Some(Match(str)) =>
//          val rest = state.drop(str.length)
//          (rest, ParseSuccess(rest).asInstanceOf[A])
//        case None =>
//          (state, ParseFailure().asInstanceOf[A])
//
//    })
//
//  def apply: FunctionK[GrammarA, ParserInterpreterState] =
//    new FunctionK[GrammarA, ParserInterpreterState] {
//      def apply[A](fa: GrammarA[A]): ParserInterpreterState[A] =
//        fa match {
//          case Regx(regexp) => parseRegex(regexp)
//          case Optional(b)  => parseOptional(b.foldMap(this))
//          case m @ Multi(g) =>
//            def x: State[String, A] =
//              State.apply(state => {
//                g.foldMap(this)
//                  .run(state)
//                  .map {
//                    case (s, ParseSuccess(_)) => x.run(s).value
//                    case (s, ParseFailure())  => (s, ParseSuccess(s).asInstanceOf[A])
//                    case (_, _)               => ???
//                  }
//                  .value
//              })
//            x
//          case Choice(a, b) =>
//            State.apply(state => {
//              val runA = a.foldMap(this).run(state).value
//              if (runA._2.asInstanceOf[ParseResult[_]].isSuccess)
//                runA
//              else {
//                b.foldMap(this).run(state).value
//              }
//            })
//        }
//    }
}
