package org.example.algorithms.trees

import java.util.NoSuchElementException
import scala.annotation.tailrec

object AVLTree {

  class AlreadyExistsException extends Exception

  abstract class Tree[+T <% Ordered[T]] extends Iterable[T] {

    def getOr[U >: T](default: U): U

    def getOr[U, TT >: T](default: U, f: NonEmptyTree[TT] => U): U

    def _left = asInstanceOf[NonEmptyTree[T]].left

    def _right = asInstanceOf[NonEmptyTree[T]].right

    def _elem = asInstanceOf[NonEmptyTree[T]].elem

  }

  case object Empty extends Tree[Nothing] {
    override def getOr[U >: Nothing](default: U): U = default

    override def getOr[U, TT >: Nothing](default: U, f: (NonEmptyTree[TT]) => U): U = default

    override def toString = ""

    override def iterator: Iterator[Nothing] = Iterator.empty
  }

  case class NonEmptyTree[+T <% Ordered[T]](elem: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

    private[AVLTree] val depthValue: Int = (depth(left) max depth(right)) + 1

    override def getOr[U, TT >: T](default: U, f: (NonEmptyTree[TT]) => U): U = f(this)

    override def getOr[U >: T](default: U): U = elem

    override def toString = s"< $left ]$elem[ $right >"

    def isLeaf = left == Empty && right == Empty

    override def iterator: Iterator[T] = left.iterator ++ Iterator(elem) ++ right.iterator
  }

  private def balance[T](t: Tree[T]) = t.getOr(0, (t: NonEmptyTree[T]) => depth(t left) - depth(t right))

  private def depth[T](tree: Tree[T]): Int = tree.getOr(0, (t: NonEmptyTree[T]) => t.depthValue)

  def Root[T <% Ordered[T]](element: T) = NonEmptyTree(element, Empty, Empty)

  def insert[T <% Ordered[T]](tree: Tree[T], elem: T): NonEmptyTree[T] =
    tree match {
      case Empty => Root(elem)
      case tree: NonEmptyTree[T] => rebalance(
        elem.compareTo(tree.elem) match {
          case -1 => NonEmptyTree(tree.elem, insert(tree.left, elem), tree.right)
          case 0 => throw new AlreadyExistsException
          case 1 => NonEmptyTree(tree.elem, tree.left, insert(tree.right, elem))
        }
      )
    }

  @tailrec
  def min[T <% Ordered[T]](tree: NonEmptyTree[T]): NonEmptyTree[T] = tree.left match {
    case NonEmptyTree(_, l: NonEmptyTree[T], _) => min(l)
    case _ => tree
  }

  def max[T <% Ordered[T]](tree: NonEmptyTree[T]): NonEmptyTree[T] = tree.right match {
    case NonEmptyTree(_, _, r: NonEmptyTree[T]) => min(r)
    case _ => tree
  }


  private def removeElem[T <% Ordered[T]](tree: Tree[T], element: T): Tree[T] = tree match {
    case Empty => throw new NoSuchElementException()
    case t@NonEmptyTree(e, l, r) => element compareTo e match {
      case 1 => NonEmptyTree(e, l, remove(r, element))
      case -1 => NonEmptyTree(e, remove(l, element), r)
      case 0 => (l, r) match {
        case (Empty, Empty) => Empty
        case (Empty, r: NonEmptyTree[T]) => r
        case (l: NonEmptyTree[T], Empty) => l
        case (l: NonEmptyTree[T], r: NonEmptyTree[T]) =>
          val substElem = min(r).elem
          val newR = remove(r, substElem)
          NonEmptyTree(substElem, l, newR)
      }
    }
  }

  def remove[T <% Ordered[T]](tree: Tree[T], element: T): Tree[T] = removeElem(tree, element) match {
    case Empty => Empty
    case ne: NonEmptyTree[T] => rebalance(ne)
  }


  private def rebalance[T <% Ordered[T]](t: NonEmptyTree[T]): NonEmptyTree[T] =
    balance(t) match {
      case -1 | 0 | 1 => t
      case 2 => if (balance(t.left) == -1) leftRight(t) else leftLeft(t)
      case -2 => if (balance(t.right) == 1) rightLeft(t) else rightRight(t)
      case _ => throw new IllegalStateException("Balance factor can not be " + balance(t))
    }


  private def leftRight[T <% Ordered[T]](tree: NonEmptyTree[T]): NonEmptyTree[T] = {
    val A = tree.left._left
    val B = tree.left._right._left
    val C = tree.left._right._right
    val D = tree.right
    val newLeft = NonEmptyTree(tree.left._elem, A, B)
    val newRight = NonEmptyTree(tree.elem, C, D)
    NonEmptyTree(tree.left._right._elem, newLeft, newRight)
  }

  private def leftLeft[T <% Ordered[T]](tree: NonEmptyTree[T]): NonEmptyTree[T] = {
    val A = tree.left._left._left
    val B = tree.left._left._right
    val C = tree.left._right
    val D = tree.right
    val newLeft = NonEmptyTree(tree.left._left._elem, A, B)
    val newRight = NonEmptyTree(tree.elem, C, D)
    NonEmptyTree(tree.left._elem, newLeft, newRight)
  }

  private def rightLeft[T <% Ordered[T]](tree: NonEmptyTree[T]): NonEmptyTree[T] = {
    val A = tree.left
    val B = tree.right._left._left
    val C = tree.right._left._right
    val D = tree.right._right
    val newLeft = NonEmptyTree(tree.elem, A, B)
    val newRight = NonEmptyTree(tree._right._elem, C, D)
    NonEmptyTree(tree.right._left._elem, newLeft, newRight)
  }

  private def rightRight[T <% Ordered[T]](tree: NonEmptyTree[T]): NonEmptyTree[T] = {
    val A = tree.left
    val B = tree.right._left
    val C = tree.right._right._left
    val D = tree.right._right._right
    val newLeft = NonEmptyTree(tree.elem, A, B)
    val newRight = NonEmptyTree(tree.right._right._elem, C, D)
    NonEmptyTree(tree.right._elem, newLeft, newRight)
  }


  def apply[T <% Ordered[T]](x: T, elems: T*): NonEmptyTree[T] = {
    var t = Root(x)
    for (elem <- elems)
      t = insert(t, elem)
    t
  }
}
