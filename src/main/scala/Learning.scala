import scala.annotation.tailrec

object Learning {

  object Chapter2 {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      a => b => f(a, b)
    }

    def compose[A, B, C](f: B => C, g: A => B): A => C = {

      g andThen f
    }
  }

  object Chapter3 {

    sealed trait List[+A]

    case object Nil extends List[Nothing]

    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    object List {
      def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(head, tail) => head + sum(tail)
      }

      def product(doubleList: List[Double]): Double = doubleList match {
        case Nil => 1
        case Cons(0, _) => 0.0
        case Cons(head, tail) => head * product(tail)
      }

      def tail[A](aList: List[A]): List[A] = {
        aList match {
          case Nil => Nil
          case Cons(_, tail) => tail
        }
      }

      def setHead[A](elem: A, aList: List[A]): List[A] = {
        Cons(elem, aList)
      }

      def drop[A](l: List[A], n: Int): List[A] = {
        n match {
          case 0 => l
          case _ => drop(tail(l), n -1)
        }
      }

      def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

        l match {
          case Nil => Nil
          case Cons(h, t) => if (f(h)) {
            t
          } else {
            dropWhile(tail(l), f)
          }
        }
      }

      def init[A](l: List[A]): List[A] = {

        l match {
          case Nil => Nil
          case Cons(_, Nil) => Nil
          case Cons(head, _) => Cons(head, init(l))
        }
      }

      def apply[A](as: A*): List[A] = {
        if (as.isEmpty) {
          Nil
        } else {
          Cons(as.head, apply(as.tail: _*))
        }
      }

      def foldRight[A,B](aList: List[A], z: B)(f: (A, B) => B): B ={
        aList match {
          case Nil => z
          case Cons(a, b) => f(a, foldRight(b, z) (f))
        }
      }

      def sum2(sumList: List[Int]): Int = {

        foldRight(sumList, 0)(_ + _)
      }

      def product2(productList: List[Double]): Double = {
        foldRight(productList, 1.0)(_ * _)
      }

      def length[A](as: List[A]): Int = {
        foldRight(as, 0)((_, b) => b + 1)
      }

      def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

        as match {
          case Nil => z
          case Cons(a, b) => foldLeft(b, f(z, a))(f)
        }
      }

      def sum3(sumList: List[Int]): Int = {

        foldLeft(sumList, 0)(_ + _)
      }

      def product3(productList: List[Double]): Double = {
        foldLeft(productList, 1.0)(_ * _)
      }

      def sum(x: Int): Int = {
        x match {
          case 0 => x
          case _ => sum(x - 1) + x
        }
      }

      @tailrec
      def tailSum(x: Int, res: Int): Int = {
        x match {
          case 0 => res
          case _ => tailSum(x - 1, x + res)
        }
      }

      def reverse[A]( alist: List[A]): List[A] = {

        foldLeft(alist, Nil : List[A])( (b, a) => setHead(a, b) )
      }

      def foldRight2[A,B](aList: List[A], z: B)(f: (A, B) => B): B ={

        foldLeft(aList, z)((b, a) => f(a, b))
      }

      def append[A](elem: A, list: List[A]): List[A] = {

        foldLeft(list, List(elem))( (b, a) => a match {
          case Nil => List(b)
          case _ => a
        })
      }

      def joinList[A](listA: List[A], listB: List[A]): List[A] = {

        listB match {
          case Nil => listA
          case Cons(h, Nil) =>
        }
      }
    }
  }

}
