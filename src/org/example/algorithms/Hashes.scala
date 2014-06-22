package org.example.algorithms

trait HashFunction

/**
 * Is calculating hash for an integer sequence, left to right.
 * @param q multiplier
 * @param modulo prime modulo
 */
class PolynomialHash(val q:Int, val modulo: Int) {
  private def mixHash(hash:Int, elem: Int)  = ((hash + elem % modulo) % modulo * q) % modulo

  def apply(sequence:Seq[Int]): Int = sequence.foldLeft(0)( mixHash )

  def iterate(sequence:Seq[Int]) : Array[Int] = {
    var res = new Array[Int](sequence.length)
    res(0) = mixHash(0,sequence(0))
    for (i <- 1 to sequence.length-1) res(i) = mixHash(res(i-1), sequence(i))
    res
  }
}

object Hashes {
  type HashFunction[A] = A => Int

}