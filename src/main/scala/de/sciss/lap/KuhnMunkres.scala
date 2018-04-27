package de.sciss.lap

import java.util

// adapted from https://raw.githubusercontent.com/hrldcpr/hungarian/master/asp.cpp
// which in turn is adapted from Dominic Battre's implementation of the Hungarian algorithm by hbc@mit.edu
// original license is MIT
object KuhnMunkres {
  def apply(matrix: Array[Array[Double]], colMap: Array[Int], rowMap: Array[Int], inf: Double = 10000.0): Unit = {
    val m = rowMap.length
    val n = colMap.length
    require (m == n)

    var k: Int = 0
    var l: Int = 0
    var j: Int = 0
    var s: Double = 0.0

    var t: Int = 0
    var q: Int = 0

    var unmatched: Int = 0

    util.Arrays.fill(colMap, 0, n, 0)
    util.Arrays.fill(rowMap, 0, m, 0)

    val parent_row  = new Array[Int   ](n)
    val unchosen_row= new Array[Int   ](m)
    val row_dec     = new Array[Double](m)
    val col_inc     = new Array[Double](n)
    val slack       = new Array[Double](n)
    val slack_row   = new Array[Int   ](n)

    // Do heuristic
    l = 0
    while (l < n) {
      // find s, the minimum element in column n
      s = matrix(0)(l)
      k = 1
      while (k < n) {
        if (matrix(k)(l) < s) s = matrix(k)(l)
        k += 1
      }
      // if s is not zero, subtract s from the column
      if (s != 0) {
        k = 0
        while (k < n) {
          matrix(k)(l) -= s
          k += 1
        }
      }
      l += 1
    }

    // initialize
    l = 0
    while (l < n) {
      rowMap(l)   = -1
      parent_row(l) = -1
      col_inc(l)    = 0.0
      slack(l)      = inf
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
      row_dec(k) = s
      l = 0
      while (l < n && !((s == row(l)) && (rowMap(l) < 0))) l += 1
      if (l < n) {
        colMap(k) = l
        rowMap(l) = k
      } else {
        colMap(k) = -1
        unchosen_row(t) = k; t += 1
      }
      k += 1
    }

    if (t > 0) {
      unmatched = t
      while ({
        q = 0
        while ({
          while (q < t && {
            k = unchosen_row(q)
            s = row_dec(k)
            l = 0
            while (l < n && {
              !((slack(l) != 0) && {
                val del = matrix(k)(l) - s + col_inc(l)
                (del < slack(l)) && {
                  if (del == 0) {
                    (rowMap(l) < 0) || { // go to break-through
                      slack(l) = 0.0
                      parent_row(l) = k
                      unchosen_row(t) = rowMap(l)
                      t += 1
                      false
                    }
                  } else {
                    slack(l) = del
                    slack_row(l) = k
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
            s = inf
            l = 0
            while (l < n) {
              if (slack(l) != 0 && slack(l) < s) {
                s = slack(l)
              }
              l += 1
            }

            q = 0
            while (q < t) {
              row_dec(unchosen_row(q)) += s
              q += 1
            }
            l = 0
            while (l < n && {
              if (slack(l) != 0) {
                slack(l) -= s
                !(slack(l) == 0 && {
                  k = slack_row(l)
                  val rm = rowMap(l)
                  if (rm < 0) {
                    j = l + 1
                    while (j < n) {
                      if (slack(j) == 0) col_inc(j) += s
                      j += 1
                    }
                    true // go to break-through
                  } else {
                    parent_row(l) = k
                    unchosen_row(t) = rm
                    t += 1
                    false
                  }
                })
              } else {
                col_inc(l) += s
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
          k = parent_row(j)
          l = j
        }
        unmatched -= 1
        unmatched != 0
      }) {
        t = 0
        l = 0
        while (l < n) {
          parent_row(l) = -1
          slack(l) = inf
          l += 1
        }
        k = 0
        while (k < m) {
          if (colMap(k) < 0) {
            unchosen_row(t) = k; t += 1
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
      while (l < n) {
        row(l) = row(l) - row_dec(k) + col_inc(l)
        l += 1
      }
      k += 1
    }
  }
}
