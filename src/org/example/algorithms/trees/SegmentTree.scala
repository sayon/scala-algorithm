package org.example.algorithms.trees

import scala.reflect.ClassTag


class MyEnum extends Enum[MyEnum]("", 1)

object SegmentTree {

  class Tree[T <% Ordered[T] : ClassTag](arr: Array[T], val fun: (T, T) => T) {

    type Node = Int

    private[this] val data = new Array[T](arr.length * 4)
    private[this] var size = 0

    private[this] def root = 1

    private[this] def value(n: Node): T = data(n)

    private[this] def setValue(n: Node, v: T) = data(n) = v

    private[this] def left(n: Node): Node = n * 2

    private[this] def right(n: Node): Node = n * 2 + 1

    private[this] def middle(l: Int, r: Int) = {
      val distance = r - l + 1
      var acc = 1
      while (acc < distance)
        acc *= 2
      l + acc / 2 - 1
    }

    private[this] def build(from: Int, to: Int, node: Node): Unit =
      if (from == to) setValue(node, arr(from - 1))
      else if (from < to) {
        val m = middle(from, to)

        val l = left(node)
        val r = right(node)
        build(from, m, l)
        build(m + 1, to, r)
        setValue(node, fun(value(l), value(r)))
      }

    build(1, arr.length, root)


    private[this] def query(node: Node, nodeLeft: Int, nodeRight: Int, queryLeft: Int, queryRight: Int): T =
      if (queryLeft == nodeLeft && queryRight == nodeRight) value(node)
      else {
        val m = middle(nodeLeft, nodeRight)
        val (ll, lr) = (nodeLeft, m)
        val (rl, rr) = (m + 1, nodeRight)

        if (queryRight <= lr) query(left(node), ll, lr, queryLeft, queryRight)
        else if (queryLeft >= rl) query(right(node), rl, rr, queryLeft, queryRight)
        else if (queryRight == rr && queryLeft <= lr) {
          val leftVal = query(left(node), ll, lr, queryLeft, lr)
          fun(leftVal, value(right(node)))
        }
        else if (queryLeft == ll && queryRight >= rl) {
          val rightVal = query(right(node), rl, rr, rl, queryRight)
          fun(value(left(node)), rightVal)
        }
        else fun(
          query(left(node), ll, lr, queryLeft, lr),
          query(right(node), rl, rr, rl, queryRight))
      }

    def apply(from: Int, to: Int): T =
      query(root, 1, arr.length, from + 1, to + 1)

  }

}

//
//  abstract class Tree[+T]
//
//  case object Empty extends Tree[Nothing]
//
//  abstract case class NonEmptyTree[+T](f: T => T => T) extends Tree[T]
//
//  case class Leaf[T](value: T)(implicit f: T => T => T) extends NonEmptyTree[T](f)
//  case class NonLeaf[T](value: T, l:NonEmptyTree[T], r: NonEmptyTree[T])(implicit f: T => T => T) extends NonEmptyTree[T](f)
//  case class RightOnlyNode[T](value: T, r: NonEmptyTree[T])(implicit f: T => T => T) extends NonEmptyTree[T](f)
//  case class LeftOnlyNode[T](value: T, r: NonEmptyTree[T])(implicit f: T => T => T) extends NonEmptyTree[T](f)


//  abstract class Tree[+T]
//  abstract class Tree[+T]
//
//  case object Empty extends Tree[Nothing]
//
//  case class NonEmptyTree[+T](v: T, from: Int, to: Int, l: Tree[T], r: Tree[T])(implicit f: (T, T) => T) extends Tree[T]
//
//  def build[T](op: (T, T) => T, array: Array[T]): Array[T] = {
//    val tree = new Array[T](4 * array.length)
//
//    def w[T](v: Int = 1, tl: Int = 0, tr: Int = array.size - 1): Unit = if (tl != tr)  {
//        val m = (tl + tr) / 2
//        w(v * 2, tl, m)
//        w(v * 2 + 1, m + 1, tr)
//        tree(v) = op(tree(v * 2), tree(v * 2 + 1))
//      }
//    w()
//    tree
//  }
//
//  def sum[T](tree: Array[T], )
//
//  //
//  //  def decompose[T](l:Int, r:Int, ll:Int, rr:Int, array: Array[T]): Tree[T] = {
//  //    val m = (l + r) /2
//  //
//  //  }
//  //
//  //  private def pairs[T](s:List[T], leftover : Option[T] = None): (List[(T,T)], Option[T]) = s match {
//  //    case Nil => (Nil, leftover)
//  //    case s1::s2::rest::Nil => (List((s1,s2)), Some(rest))
//  //    case s1::s2:: ss =>
//  //      val rest = pairs(ss, leftover)
//  //      ( (s1,s2)::rest._1, rest._2)
//  //  }
//  //  def build[T](sequence: Seq[T]): Tree[T] = {
//  //    var segmentSize = 1
//  //
//  //    while( segmentSize <= sequence.size) {
//  //      pairs(sequence)
//  //    }
//  //  }
//
