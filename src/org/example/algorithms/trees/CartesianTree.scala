package org.example.algorithms.trees

object CleverTree {

  abstract class Tree[+T]

  abstract class NonEmptyTree[+T](x: T) extends Tree[T]

  case object EmptyTree extends Tree[Nothing]

  case class NonLeaf[T](x: T, l: Tree[T], r: Tree[T]) extends NonEmptyTree[T](x)

  case class Leaf[T](x: T) extends NonEmptyTree[T](x)

}

object CartesianTree {

  abstract class Tree[+K <% Ordered[K], +V <% Ordered[V]] extends Iterable[(K, V)] {
    def +[
    K1 >: K <% Ordered[K1],
    V1 >: V <% Ordered[V1]
    ](kvp: (K1, V1)): NonEmptyTree[K1, V1] = add(this, kvp)

    def -[K1 >: K](k: K1): Tree[K, V] = ???
  }

  case object Empty extends Tree[Nothing, Nothing] {
    override def iterator: Iterator[(Nothing, Nothing)] = Iterator.empty
    override def toString() = ""
  }

  case class NonEmptyTree[+K <% Ordered[K], +V <% Ordered[V]](key: K, value: V, left: Tree[K, V], right: Tree[K, V]) extends Tree[K, V] {
    override def iterator: Iterator[(K, V)] = left.iterator ++ Iterator((key, value)) ++ right.iterator
    override def toString() = s"<$left ]$key, $value[ $right>"
  }

  def Leaf[K <% Ordered[K], V <% Ordered[V]](k: K, v: V) = NonEmptyTree(k, v, Empty, Empty)

  def merge[K <% Ordered[K], V <% Ordered[V]](l: Tree[K, V], r: Tree[K, V]): Tree[K, V] =
    (l, r) match {
      case (l, Empty) => l
      case (Empty, r) => r
      case (l@NonEmptyTree(lk, lv, ll, lr), r@NonEmptyTree(rk, rv, _, _)) if lv > rv =>
        NonEmptyTree(lk, lv, ll, merge(lr, r))
      case (l@NonEmptyTree(lk, lv, _, _), r@NonEmptyTree(rk, rv, rl, rr)) if lv <= rv =>
        NonEmptyTree(rk, rv, merge(l, rl), rr)
    }


  def split[K <% Ordered[K], V <% Ordered[V]](t: Tree[K, V], splitBy: K): (Tree[K, V], Tree[K, V]) = t match {
    case Empty => (Empty, Empty)
    case t@NonEmptyTree(k, v, l, r) if k <= splitBy =>
      val rightSplitted = split(r, splitBy)
      (NonEmptyTree(k, v, l, rightSplitted._1), rightSplitted._2)
    case t@NonEmptyTree(k, v, l, r) if k > splitBy =>
      val leftSplitted = split(l, splitBy)
      (leftSplitted._1, NonEmptyTree(k, v, leftSplitted._2, r))
  }


  def add[K <% Ordered[K], V <% Ordered[V]](t: Tree[K, V], kvp: (K, V)): NonEmptyTree[K, V] = t match {
    case Empty => Leaf(kvp._1, kvp._2)
    case t@NonEmptyTree(k, v, l, r) =>
      val splitted = split[K, V](t, kvp._1)
      merge(merge(splitted._1, Leaf(kvp._1, kvp._2)), splitted._2)
        .asInstanceOf[NonEmptyTree[K, V]] //our types can not deduce that merge with a leaf is never empty
  }

  def remove[K <% Ordered[K], V <% Ordered[V]](tree: NonEmptyTree[K, V], key: K) = ???

  /**
   * The following shows that it is possible to build cartesian tree for any input
   */
  def buildSquareTime[K <% Ordered[K], V <% Ordered[V]](x: (K,V), xs: (K, V)*): Tree[K, V] = {

    def build(data: List[(K, V)]): Tree[K, V] = data match {
      case Nil => Empty
      case (k, v) :: Nil => Leaf(k, v)
      case lst => // only this one is of interest
        val maxY = data.maxBy(_._2)
        val left = data.filter(_._1 < maxY._1)
        val right = data.filter(_._1 > maxY._1)
        NonEmptyTree(maxY._1, maxY._2, build(left), build(right))
    }
    //don't forget to sort
    build((x::xs.toList).sortBy(_._1))
  }


  /*
  It is also possible to construct cartesian tree in a linear time, tho not in an immutable way.
  * */
}
