import java.util.regex.Pattern

import cats.syntax.apply

import scala.util.Random

object Example extends App {

  import DSL._

  implicit val rnd = new Random(13370)

  val nouns: Grammar[String] =
    readGrammarFile("src/main/resources/nouns.txt")
  val verbs: Grammar[String] =
    readGrammarFile("src/main/resources/verbs.txt")
  val adverbs: Grammar[String] =
    readGrammarFile("src/main/resources/adverbs.txt")
  val adjectives: Grammar[String] =
    readGrammarFile("src/main/resources/adjectives.txt")


  def readGrammarFile(path: String): Grammar[String] =
    rnd.shuffle(scala.io.Source.fromFile(path).getLines().toList).take(200).filter(!_.contains("-")).mkString("|")

  implicit def toGrammar(s: String): Grammar[String] = Regx[String](s.r)

  for (_ <- 0 to 100) {
    otherText
  }

  def otherText = {
    val txt =
      s"""
        |Look at that ${oneOf(adjectives)}, ${oneOf((adjectives ~ ", ").*)}${oneOf(nouns)}. Someone call for ${oneOf(nouns)}!
        |Could you ${oneOf(adverbs)} ${oneOf(verbs)} the ${oneOf(nouns)} for me?
      """.stripMargin
    println(txt)
  }

  def helloText: Unit = {

    val text =
      s"""
         |Hello ${oneOf(nouns).capitalize}, you look ${oneOf(adverbs)} ${oneOf(adjectives)}
 today. Would you
         |like to see ${oneOf(adverbs)}, ${oneOf(adverbs)} ${oneOf(adjectives)} ${oneOf(nouns)}?
      """.stripMargin
    println(text)
  }

  def oneOf[T](g: Grammar[T]) = generate(g).result.toLowerCase

}
