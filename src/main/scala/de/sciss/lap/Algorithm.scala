package de.sciss.lap

trait Algorithm {
  def algorithmName: String

  def solveLAP(matrix: Array[Array[Float]], colMap: Array[Int], rowMap: Array[Int],
               progress: (Int, Int) => Unit = null): Unit
}
