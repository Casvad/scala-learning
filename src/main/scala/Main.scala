import Learning.Chapter3.List.{reverse, sum}
import Learning.Chapter3.{Cons, List, Nil}

object Main {

  def main(args: Array[String]): Unit = {

    val x = List(1, 2, 3, 4, 5)

    val y = reverse(x)

    println(y)
  }
}
