import Chapter3.List.setHead

import scala.annotation.tailrec

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
        case Cons(head, tail) => Cons(head, init(tail))
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

      reverse(setHead(elem, reverse(list)))
    }

    def append2[A](elem: A, list: List[A]): List[A] = {

      list match {
        case Nil => List(elem)
        case Cons(h, t) => Cons(h, append2(elem, t))
      }
    }

    def appendReverse[A](elem: A, list: List[A]): List[A] = {
      foldLeft(list, List(elem))((b, a) => Cons(a, b))
    }

    def append4[A](elem: A, list: List[A]): List[A] = {

      foldRight(list, List(elem))( (a,b) => Cons(a, b))
    }

    def concat[A](alist: List[A], blist: List[A]): List[A] = {

      foldRight(alist, blist)((a,b) => Cons(a, b))
    }

    def addOne(list: List[Int]): List[Int] = {

      list match {
        case Nil => Nil
        case Cons(h , t) => Cons(h + 1, addOne(t))
      }
    }

    def toStringList(list: List[Double]): List[String] = {

      list match {
        case Nil => Nil: List[String]
        case Cons(h , t) => Cons(h.toString, toStringList(t))
      }
    }

    def map[A,B](as: List[A])(f: A => B): List[B] = {

      as match {
        case Nil => Nil
        case Cons(a, b) => Cons(f(a), map(b)(f))
      }
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {

      as match {
        case Nil => Nil
        case Cons(a, b) => if (f(a)) {
          Cons(a, filter(b)(f))
        } else {
          filter(b)(f)
        }
      }
    }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {

      as match {
        case Nil => Nil
        case Cons(a,b) => concat(f(a), flatMap(b)(f))
      }
    }

    def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {

      flatMap(as)(a => {
        if (f(a)){
          List(a)
        }else Nil
      })
    }

    def zipSum(list1: List[Int], list2: List[Int]): List[Int] = {

      list1 match {
        case Nil => Nil
        case Cons(h, t) =>
          list2 match {
            case Nil => Nil
            case Cons(h2, t2) => Cons(h + h2, zipSum(t, t2))
          }
      }
    }

    def zipWith[A,B,C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = {

      list1 match {
        case Nil => Nil
        case Cons(h, t) =>
          list2 match {
            case Nil => Nil
            case Cons(h2, t2) => Cons(f(h, h2), zipWith(t, t2)(f))
          }
      }
    }

    def zipSum2(list1: List[Int], list2: List[Int]): List[Int] = {
      zipWith(list1, list2)(_ + _)
    }

    def hasSubSequence[A](list1: List[A], list2: List[A]): Boolean = {
        list2 match {
          case Nil => true
          case Cons(h, t) =>
            list1 match {
              case Nil => false
              case Cons(h2, t2) => if (h2 == h){
                hasSubSequence(t2, t)
              }else {
                false //todo restart list when is false
              }
            }
        }
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size[A](t: Tree[A]): Int = {

      t match {
        case Leaf(value) => 1
        case Branch(left, right) =>size(left) + size(right)
      }
    }
  }
}