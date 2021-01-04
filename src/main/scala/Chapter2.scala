import scala.annotation.tailrec

object Chapter2 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {

    g andThen f
  }

}
