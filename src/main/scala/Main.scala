
import Chapter3.List.{addOne, concat, filterFlatMap, flatMap, toStringList, zipSum, zipSum2}
import Chapter3.List

object Main {

  def main(args: Array[String]): Unit = {

    val x: List[Int] = List(1, 2, 3)

    val y: List[Int] = List(7,8,9)
    val z: List[Double] = List(0.0, 1.1)

//    println(toStringList(z))
//
//    println(flatMap(List(1,2,3))(i => List(i,i)))
//    println(filterFlatMap(List(1,2,3))(i => i == 2))

    println(zipSum2(x, y))
  }
}
