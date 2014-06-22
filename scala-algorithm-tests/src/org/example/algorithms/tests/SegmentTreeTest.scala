package org.example.algorithms.tests

import org.example.algorithms.trees.SegmentTree
import org.scalatest.FlatSpec

class SegmentTreeTest extends FlatSpec {

  "A segment tree" should "return each element when queried a segment of length one" in {
    val t = new SegmentTree.Tree[Int](Array(1, 2, 3, 4, 5), {
      _ min _
    })
    for (i <- 0 to 4) {
      assert(i + 1 == t(i, i))
    }
  }
  "A segment tree" should "work correctly for all segments" in {
    val arr = Array(1, 2, 3, 4, 5)
    val t = new SegmentTree.Tree[Int](arr, {
      _ min _
    })

    for (i <- 0 to 4; j <- i to 4) {
      assert(arr.slice(i, j + 1).min == t(i, j))
    }
  }
}
