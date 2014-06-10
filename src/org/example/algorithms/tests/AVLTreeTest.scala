package org.example.algorithms.tests


import org.junit._
import org.example.algorithms.trees.AVLTree
import Assert._

class AVLTreeTest {
import AVLTree._
  @Test
  def add() = {
    val t = AVLTree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    assertEquals(depth(t), 2)
  }
}

