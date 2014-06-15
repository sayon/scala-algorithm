package org.example.algorithms.tests

import org.scalatest.FlatSpec
import org.example.algorithms.trees.CartesianTree
import CartesianTree._

class CartesianTreeTest extends FlatSpec {
  "A treap" should "be initialized correctly" in {
    val t = buildSquareTime ((1, 'a'), (2, 'b'), (3, 'r'), (4, 'w'))
    assert(t.toString == "<<<< ]1, a[ > ]2, b[ > ]3, r[ > ]4, w[ >")
  }

  "A treap" should "be splitted correctly" in {
    val t = buildSquareTime ((1, 'a'), (2, 'b'), (3, 'r'), (4, 'w'))
    val (l,r) = split(t, 3)
    assert(l.toString() == "<<< ]1, a[ > ]2, b[ > ]3, r[ >")
    assert(r.toString() == "< ]4, w[ >")

  }

}
