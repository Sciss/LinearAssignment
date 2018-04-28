/*
 *  JonkerVolgenant.scala
 *  (LinearAssignment)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lap

// adapted from https://github.com/hrldcpr/pyLAPJV/blob/master/lap.cpp

/************************************************************************
  *
  *  lap.cpp
   version 1.0 - 4 September 1996
   author: Roy Jonker @ MagicLogic Optimization Inc.
   e-mail: roy_jonker@magiclogic.com

   Code for Linear Assignment Problem, according to

   "A Shortest Augmenting Path Algorithm for Dense and Sparse Linear
    Assignment Problems," Computing 38, 325-340, 1987

   by

   R. Jonker and A. Volgenant, University of Amsterdam.

  *
CHANGED 2004-08-13 by Harold Cooper (hbc@mit.edu) for the pyLAPJV Python module:
      -- commented out system.h and checklap()
  *
  *************************************************************************/

object JonkerVolgenant extends Algorithm {
  def algorithmName: String = "Jonker-Volgenant"

  def solveLAP(matrix: Array[Array[Float]], rowMap: Array[Int], colMap: Array[Int],
               progress: (Int, Int) => Unit = null): Unit = {
    if (progress != null) Console.err.println(s"$algorithmName: progress not yet supported.")
    apply(matrix, rowMap = rowMap, colMap = colMap)
  }

  /** Given an n x n cost `matrix`, determines the best 1 to 1 mapping between rows and columns.
    * Returns the total cost.
    *
    * @param matrix   matrix with costs; costs must be greater than or equal to zero.
    *                 rows and columns must be of size `rowMap.length` and `colMap.length`
    * @param colMap   will be filled with the result, mapping columns (array indices) to rows (array values)
    * @param rowMap   will be filled with the result, mapping rows (array indices) to columns (array values).
    *                 must have the same size as `colMap`.
    */
  def apply(matrix: Array[Array[Float]], rowMap: Array[Int], colMap: Array[Int]): Float = {
    val n = rowMap.length
    require (n == colMap.length)

    val u = new Array[Float](n)
    val v = new Array[Float](n)

    var unassignedFound = false

    var i           : Int = 0
    var iMin        : Int = 0
    var numFree     : Int = 0
    var prevNumFree : Int = 0
    var f           : Int = 0
    var i0          : Int = 0
    var k           : Int = 0
    var freeRow     : Int = 0

    var j           : Int = 0
    var j1          : Int = 0
    var j2          : Int = 0
    var endOfPath   : Int = 0
    var last        : Int = 0
    var low         : Int = 0
    var up          : Int = 0

    var min         : Float = 0f
    var h           : Float = 0f
    var uMin        : Float = 0f
    var uSubMin     : Float = 0f
    var v2          : Float = 0f

    val free    = new Array[Int   ](n)    // list of unassigned rows.
    val colList = new Array[Int   ](n)    // list of columns to be scanned in various ways.
    val matches = new Array[Int   ](n)    // counts how many times a row could be assigned.
    val d       = new Array[Float ](n)    // 'cost-distance' in augmenting path calculation.
    val pred    = new Array[Int   ](n)    // row-predecessor of column in augmenting/alternating path.

    var max = 1.0f

    // COLUMN REDUCTION
    j = n - 1
    while (j >= 0) {    // reverse order gives better results.
      // find minimum cost over rows.
      min = matrix(0)(j)
      if (min > max) max = min
      iMin = 0
      i = 1
      while (i < n) {
        val c = matrix(i)(j)
        if (c < min) {
          min = c
          iMin = i
        } else if (c > max) max = c
        i += 1
      }
      v(j) = min

      if ({ val m0 = matches(iMin) + 1; matches(iMin) = m0; m0 } == 1) {
        // init assignment if minimum row assigned for first time.
        rowMap(iMin) = j
        colMap(j) = iMin
      } else {
        colMap(j) = -1 // row already assigned, column not assigned.
      }
      j -= 1
    }

//    val BIG = 100000f
    val BIG = max * 2 // XXX TODO --- test if Float.MaxValue would work

    // REDUCTION TRANSFER
    i = 0
    while (i < n) {
      if (matches(i) == 0) { // fill list of unassigned 'free' rows.
        free(numFree) = i; numFree += 1
      } else if (matches(i) == 1) { // transfer reduction from rows that are assigned once.
        j1 = rowMap(i)
        min = BIG
        j = 0
        while (j < n) {
          if (j != j1 && matrix(i)(j) - v(j) < min) {
            min = matrix(i)(j) - v(j)
          }
          j += 1
        }
        v(j1) = v(j1) - min
      }
      i += 1
    }

    // AUGMENTING ROW REDUCTION
    var loopCnt = 0           // do-loop to be done twice.
    do {
      loopCnt += 1

      // scan all free rows.
      // in some cases, a free row may be replaced with another one to be scanned next.
      k = 0
      prevNumFree = numFree
      numFree = 0             // start list of rows still free after augmenting row reduction.
      while (k < prevNumFree) {
        i = free(k)
        k += 1

        // find minimum and second minimum reduced cost over columns.
        uMin = matrix(i)(0) - v(0)
        j1 = 0
        uSubMin = BIG
        j = 1
        while (j < n) {
          h = matrix(i)(j) - v(j)
          if (h < uSubMin) {
            if (h >= uMin) {
              uSubMin = h
              j2 = j
            } else {
              uSubMin = uMin
              uMin = h
              j2 = j1
              j1 = j
            }
          }
          j += 1
        }

        i0 = colMap(j1)
        if (uMin < uSubMin) {
          // change the reduction of the minimum column to increase the minimum
          // reduced cost in the row to the sub-minimum.
          v(j1) = v(j1) - (uSubMin - uMin)
        } else                   // minimum and sub-minimum equal.
        if (i0 >= 0) {        // minimum column j1 is assigned.
          // swap columns j1 and j2, as j2 may be unassigned.
          j1 = j2
          i0 = colMap(j2)
        }

        // (re-)assign i to j1, possibly de-assigning an i0.
        rowMap(i) = j1
        colMap(j1) = i

        if (i0 >= 0)           // minimum column j1 assigned earlier.
          if (uMin < uSubMin) {
            // put in current k, and go back to that k.
            // continue augmenting path i - j1 with i0.
            k -= 1; free(k) = i0
          } else {
            // no further augmenting reduction possible.
            // store i0 in list of free rows for next phase.
            free(numFree) = i0; numFree += 1
          }
      }
    }
    while (loopCnt < 2)       // repeat once.

    // AUGMENT SOLUTION for each free row.
    f = 0
    while (f < numFree) {
      freeRow = free(f)       // start row of augmenting path.

      // Dijkstra shortest path algorithm.
      // runs until unassigned column added to shortest path tree.
      j = 0
      while (j < n) {
        d(j) = matrix(freeRow)(j) - v(j)
        pred(j) = freeRow
        colList(j) = j        // init column list.
        j += 1
      }

      low = 0 // columns in 0..low-1 are ready, now none.
      up = 0  // columns in low..up-1 are to be scanned for current minimum, now none.
      // columns in up..dim-1 are to be considered later to find new minimum,
      // at this stage the list simply contains all columns
      unassignedFound = false
      do {
        if (up == low) {        // no more columns to be scanned for current minimum.
          last = low - 1

          // scan columns for up..dim-1 to find all indices for which new minimum occurs.
          // store these indices between low..up-1 (increasing up).
          min = d(colList(up)); up += 1
          k = up
          while (k < n) {
            j = colList(k)
            h = d(j)
            if (h <= min) {
              if (h < min) {    // new minimum.
                up = low      // restart list at index low.
                min = h
              }
              // new index with same minimum, put on index up, and extend list.
              colList(k) = colList(up)
              colList(up) = j; up += 1
            }
            k += 1
          }

          // check if any of the minimum columns happens to be unassigned.
          // if so, we have an augmenting path right away.
          k = low
          while (k < up) {
            if (colMap(colList(k)) < 0) {
              endOfPath = colList(k)
              unassignedFound = true
              k = up // break
            } else {
              k += 1
            }
          }
        }

        if (!unassignedFound) {
          // update 'distances' between free-row and all unscanned columns, via next scanned column.
          j1 = colList(low)
          low += 1
          i = colMap(j1)
          h = matrix(i)(j1) - v(j1) - min

          k = up
          while (k < n) {
            j = colList(k)
            v2 = matrix(i)(j) - v(j) - h
            if (v2 < d(j)) {
              pred(j) = i
              if (v2 == min) { // new column found at same minimum value
                if (colMap(j) < 0) {
                  // if unassigned, shortest augmenting path is complete.
                  endOfPath = j
                  unassignedFound = true
                  k = n // break

                } else { // else add to list to be scanned right away.
                  colList(k) = colList(up)
                  colList(up) = j
                  up += 1
                  d(j) = v2
                }
              } else {
                d(j) = v2
              }
            }
            k += 1
          }
        }
      }
      while (!unassignedFound)

      // update column prices.
      k = 0
      while (k <= last) {
        j1 = colList(k)
        v(j1) = v(j1) + d(j1) - min
        k += 1
      }

      // reset row and column assignments along the alternating path.
      do {
        i = pred(endOfPath)
        colMap(endOfPath) = i
        j1 = endOfPath
        endOfPath = rowMap(i)
        rowMap(i) = j1

      } while (i != freeRow)

      f += 1
    }

    // calculate optimal cost.
    var lapCost: Float = 0f

    i = 0
    while (i < n) {
      j = rowMap(i)
      u(i) = matrix(i)(j) - v(j)
      lapCost = lapCost + matrix(i)(j)
      i += 1
    }

    lapCost
  }
}
