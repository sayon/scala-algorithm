package org.example.algorithms.trees

import scala.collection.immutable.Stack

object SuffixTree {

  case class Tree(str: String, var links: List[Tree] = Nil)

  def apply(string: String): Tree =
    suffixes(string).foldLeft(Tree("", Nil))(embrace)


  private def suffixes(string: String) = for (i <- 0 until string.length) yield string.substring(i)

  private def cut(t: Tree, i: Int) = {
    val (l, r) = t.str.splitAt(i)
    Tree(l, List(Tree(r, t.links)))
  }

  def embrace(t: Tree, suffix: String): Tree = {
    t.links.find(c => commonPrefixLen(t.str, c.str) > 0) match {
      case None => Tree(t.str, Tree(suffix) :: t.links)
      case Some(child) =>
        val cpl = commonPrefixLen(suffix, child.str)
        val subSuffix = suffix.substring(cpl)
        val cutted = cut(child, cpl)
        Tree(t.str, t.links.updated(t.links.indexOf(child), embrace(cutted, subSuffix)))
    }
  }

  private def commonPrefixLen(fst: String, snd: String): Int = {
    var l = 0
    while (l < fst.length && l < snd.length && fst(l) == snd(l)) l += 1
    l
  }

  def print(tree: Tree, stack: Stack[String] = Stack.empty[String]): Unit = {
    for (child <- tree.links)
      print(child, stack.push(child.str))

    stack.foreach(Console.print)
    println()
  }


}
