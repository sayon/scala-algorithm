package org.example.algorithms.tests

import org.junit._
import org.example.algorithms.graphs.{SimpleDirected, BellmanFord, SimpleLabeled, SimpleLabeledImpl}
import org.example.algorithms.graphs.SearchProviders._

class BellmanFordTest {
  @Test
  def creation() : Unit = {
    val g: SimpleLabeledImpl[Int] = SimpleLabeled(0 -> List((0, 3), (1, 1)),
      1 -> Nil,
      2 -> List((5, 1), (9, 3)),
      3 -> Nil
    )
    val startNode = g.vertex(3)

    println(BellmanFord(g)(startNode))
    dfs(g)(g.vertex(3))
  }

  @Test
  def test() = {
    val g = SimpleDirected(
      0 -> Set(3),
      1 -> Set.empty[Int],
      2 -> Set(1),
      4 -> Set(2),
      3 -> Set(2, 3)
    )
    g.edges foreach println
  }
}
