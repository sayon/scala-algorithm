package org.example.algorithms.trees

/**
 * Analysis. Why is it almost ok?
 *
 * Assign potential w(i) to each node i:
 * Let
 *  x               --  be a selected node
 *  s(x)            --  sum of nodes in x's subtree (its weight)
 *  r(x) = log s(x) --  rank of x, it's potential P(x)
 *  t               --  tree root
 *  potential of tree is a sum of ranks of all its vertices
 *
 * Lemma 1. With these w(i) amortized time to splay a tree with root `t` at a node `x` is
 *          less or equal to  `3r(t) - 3r(x) + 1`
 * Proof.
 * No steps - obvious
 * We will prove:
 * 1. Amortized cost of Zig is             <= 3 (r'(x) -r(x)) + 1
 * 2. Amortized cost of ZigZig and ZigZag  <= 3(r'(x) - r(x))
 *
 * Cases:                         // r'(x) is the rank after step
 * 1. Zig:
 *    1 + r'(x) + r'(p) - r(x) - r(p)     // only x and p can change rank
 *    <= 1 + r'(x) - r(x)                 // r(p) >= r'(p)
 *    ok.
 *
 * 2. ZigZig
 *    r'(x) - r(x) + r'(p) - r'(p) + r'(g) - r'(g) <=
 *    //given:  r'(x) = r(g), r'(p) <= r(x) , r(p) <= r(x), r'(g) < r'(x)
 *    <= 2* (r'(x)-r(x))
 *    So the cost C = delta Phi + 1 <= 1 + 2* (r'(x) - r(x))   (one is added for the turn itself)
 *    Is it true, that the given is in turn <= 3( r'(x) - r(x)) iff r'(x) - r(x) >= 1 holds.
 *    Prove it by contradiction:
 *
 *    Assume forall x. r'(x) - r(x) = 0  <-> forall x. r'(x) = r(x)
 *    Obviously: s'(x) <= 2^k && s'(x) < 2^(k+1)          // same is true for s(x)
 *    s'(x) = 3 + s(A) + s(B) + s(C) + s(D)  =            // look at the image
 *    //given s(x) = s(A ) + s(B) + 1), s'(g) = s(C) + s(D) + 1
 *    = s(x) + s'(g) + 1 >=
 *    >= 2^k + 2^k + 1 > 2^(k+1) , which is impossible. ok.
 *
 *
 * ZigZag proof is the same.
 *
 * Qed.


//search, insert, split, merge, erase



//fixme implement top-down splaying
object SplayTree {

  // x is the root's child
  def zig[T](x: NonEmptyTree[T], root: NonEmptyTree[T]): NonEmptyTree[T] = {
    if (root.left == x) {
      val A = x.left
      val B = x.right
      val C = root.right
      NonEmptyTree(x.elem, A, NonEmptyTree(root.elem, B, C))
    }
    else if (root.right == x) {
      val A = root.left
      val B = x.left
      val C = x.right
      NonEmptyTree(x.elem, NonEmptyTree(root.elem, A, B), C)
    }
    else throw new IllegalStateException("Zig should be performed on the root and its child")
  }

  def zigzig[T](x: NonEmptyTree[T], p: NonEmptyTree[T], g: NonEmptyTree[T]): NonEmptyTree[T] = {
    if (p.left == x && g.left == p) {
      val A = x.left
      val B = x.right
      val C = p.left
      val D = g.right
      NonEmptyTree(x.elem, A, NonEmptyTree(p.elem, B, NonEmptyTree(g.elem, C, D)))
    } else if (p.right == x && g.right == p) {
      val A = g.left
      val B = p.left
      val C = x.left
      val D = x.right
      NonEmptyTree(x.elem, NonEmptyTree(p.elem, NonEmptyTree(g.elem, A, B), C), D)
    }
    else throw new IllegalStateException("Invalid zigzig usage")
  }

  def zigzag[T](x: NonEmptyTree[T], p: NonEmptyTree[T], g: NonEmptyTree[T]): Tree[T] = {
    if (g.left == p && p.right == x) {
      val A = p.left
      val B = x.left
      val C = x.right
      val D = g.right
      x.update(p.update(A, B), g.update(C, D))
    } else if (g.right == p && p.left == x) {
      val A = g.left
      val B = x.left
      val C = x.right
      val D = p.right
      x.update(g.update(A, B), p.update(C, D))
    } else throw new IllegalStateException("Invalid zigzag usage")
  }

  def splay[T <% Ordered[T]](tree: NonEmptyTree[T], x: T): NonEmptyTree[T] = {
    def rotate(t: NonEmptyTree[T]) = t.elem.compareTo(x) match {
      case -1 => t.left match {
        case Empty =>
        case fst@NonEmptyTree(fstEl, l, r) => fstEl.compareTo(x) match {
          case -1 =>
        }
      }
      case 1 =>
      case _ => {}
    }

  }

  abstract class Tree[+T <% Ordered[T]] extends Iterable[T] {

    def getOr[U >: T](default: U): U

    def getOr[U, TT >: T](default: U, f: NonEmptyTree[TT] => U): U

    def _left = asInstanceOf[NonEmptyTree[T]].left

    def _right = asInstanceOf[NonEmptyTree[T]].right

    def _elem = asInstanceOf[NonEmptyTree[T]].elem

    def -[U >: T <% Ordered[U]](e: U) = remove(this, e)

    def +[U >: T <% Ordered[U]](e: U) = insert(this, e)
  }

  case object Empty extends Tree[Nothing] {
    override def getOr[U >: Nothing](default: U): U = default

    override def getOr[U, TT >: Nothing](default: U, f: (NonEmptyTree[TT]) => U): U = default

    override def toString = ""

    override def iterator: Iterator[Nothing] = Iterator.empty
  }

  case class NonEmptyTree[+T <% Ordered[T]](elem: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

    override def getOr[U, TT >: T](default: U, f: (NonEmptyTree[TT]) => U): U = f(this)

    override def getOr[U >: T](default: U): U = elem

    override def toString = s"< $left ]$elem[ $right >"

    def isLeaf = left == Empty && right == Empty

    override def iterator: Iterator[T] = left.iterator ++ Iterator(elem) ++ right.iterator

    def update(l: Tree[T], r: Tree[T]) =