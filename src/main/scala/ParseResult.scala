object ParseResult {}

sealed trait ParseResult[+T] {
  def isSuccess: Boolean = false
}

case class ParseSuccess[T](remaining: T) extends ParseResult[T] {
  override def isSuccess: Boolean = true
}
case class ParseFailure() extends ParseResult[Nothing]
