/*
 *  Algorithm.scala
 *  (LinearAssignment)
 *
 *  Copyright (c) 2018-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lap

trait Algorithm {
  def algorithmName: String

  def solveLAP(matrix: Array[Array[Float]], colMap: Array[Int], rowMap: Array[Int],
               progress: (Int, Int) => Unit = null): Unit
}
