/*
 *  KuhnMunkres.scala
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

import java.util

// adapted from https://raw.githubusercontent.com/hrldcpr/hungarian/master/asp.cpp
// which in turn is adapted from Dominic Battre's implementation of the Hungarian algorithm by hbc@mit.edu
// original license is MIT
object KuhnMunkres {
  /** Given an n x n cost `matrix`, determines the best 1 to 1 mapping between rows and columns.
    *
    * @param matrix   matrix with costs; costs must be greater than or equal to zero.
    *                 rows and columns must be of size `rowMap.length` and `colMap.length`
    * @param colMap   will be filled with the result, mapping columns (array indices) to rows (array values)
    * @param rowMap   will be filled with the result, mapping rows (array indices) to columns (array values).
    *                 must have the same size as `colMap`.
    */
  def apply(matrix: Array[Array[Float]], colMap: Array[Int], rowMap: Array[Int],
            progress: (Int, Int) => Unit = null): Unit = {
    val m = rowMap.length
    val n = colMap.length
    require (m == n)
    val hp = progress != null

    var k: Int    = 0
    var l: Int    = 0
    var j: Int    = 0
    var s: Float  = 0.0f
    var t: Int    = 0
    var q: Int    = 0

    var unmatched: Int = 0

    util.Arrays.fill(colMap, 0, n, 0)
    util.Arrays.fill(rowMap, 0, m, 0)

    val parentRow   = new Array[Int  ](n)
    val unchosenRow = new Array[Int  ](m)
    val rowDec      = new Array[Float](m)
    val colInc      = new Array[Float](n)
    val slack       = new Array[Float](n)
    val slackRow    = new Array[Int  ](n)

    // Do heuristic
    l = 0
    var max = 1.0f
    while (l < n) {
      // find s, the minimum element in column n
      s = matrix(0)(l)
      if (s > max) max = s
      k = 1
      while (k < n) {
        val c = matrix(k)(l)
        if (c < s) s = c
        else if (c > max) max = c
        k += 1
      }
      // if s is not zero, subtract s from the column
      if (s != 0) {
        k = 0
        while (k < n) {
          val row = matrix(k)
          row(l) -= s
          k += 1
        }
      }
      l += 1
    }
    val INF = max * 2 // XXX TODO --- test if Float.MaxValue would work

    // initialize
    l = 0
    while (l < n) {
      rowMap(l)    = -1
      parentRow(l) = -1
      colInc(l)    = 0.0f
      slack(l)     = INF
      l += 1
    }

    t = 0
    k = 0
    while (k < m) {
      val row = matrix(k)
      // calculate minimum element in row
      s = row(0)
      l = 1
      while (l < n) {
        val c = row(l)
        if (c < s) s = c
        l += 1
      }
      rowDec(k) = s
      l = 0
      while (l < n && !((s == row(l)) && (rowMap(l) < 0))) l += 1
      if (l < n) {
        colMap(k) = l
        rowMap(l) = k
      } else {
        colMap(k) = -1
        unchosenRow(t) = k; t += 1
      }
      k += 1
    }

    if (t > 0) {
      unmatched = t
      val nu = unmatched
      while ({
        q = 0 // XXX TODO --- feels odd that this is not reside before the next `while`
        while ({
          if (hp) progress(unmatched, nu)
//          println(s"unmatched = $unmatched, q = $q, t = $t")
          while (q < t && {
            k = unchosenRow(q)
            val row = matrix(k)
            s = rowDec(k)
            l = 0
            while (l < n && {
              !((slack(l) != 0) && {
                val del = row(l) - s + colInc(l)
                (del < slack(l)) && {
                  if (del == 0) {
                    val rm = rowMap(l)
                    (rm < 0) || { // go to break-through
                      slack      (l) = 0.0f
                      parentRow  (l) = k
                      unchosenRow(t) = rm
                      t += 1
                      false
                    }
                  } else {
                    slack   (l) = del
                    slackRow(l) = k
                    false
                  }
                }
              })
            }) {
              l += 1
            }
            l == n
          }) {
            q += 1
          }

          q == t && {
            s = INF
            l = 0
            while (l < n) {
              if (slack(l) != 0 && slack(l) < s) {
                s = slack(l)
              }
              l += 1
            }

            q = 0
            while (q < t) {
              rowDec(unchosenRow(q)) += s
              q += 1
            }
            l = 0
            while (l < n && {
              if (slack(l) != 0) {
                slack(l) -= s
                !(slack(l) == 0 && {
                  k = slackRow(l)
                  val rm = rowMap(l)
                  if (rm < 0) {
                    j = l + 1
                    while (j < n) {
                      if (slack(j) == 0) colInc(j) += s
                      j += 1
                    }
                    true // go to break-through
                  } else {
                    parentRow  (l) = k
                    unchosenRow(t) = rm
                    t += 1
                    false
                  }
                })
              } else {
                colInc(l) += s
                true
              }
            }) {
              l += 1
            }

            l == n
          }
        }) ()

        // break-through:
        while ({
          j = colMap(k)
          colMap(k) = l
          rowMap(l) = k
          j >= 0
        }) {
          k = parentRow(j)
          l = j
        }
        unmatched -= 1
        unmatched != 0
      }) {
        t = 0
        l = 0
        while (l < n) {
          parentRow(l) = -1
          slack    (l) = INF
          l += 1
        }
        k = 0
        while (k < m) {
          if (colMap(k) < 0) {
            unchosenRow(t) = k; t += 1
          }
          k += 1
        }
      }
    }

    // finalize result
    k = 0
    while (k < m) {
      l = 0
      val row = matrix(k)
      val rd  = rowDec(k)
      while (l < n) {
        row(l) = row(l) - rd + colInc(l)
        l += 1
      }
      k += 1
    }
  }
}
