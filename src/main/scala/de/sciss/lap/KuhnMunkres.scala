package de.sciss.lap

import java.util

// adapted from https://raw.githubusercontent.com/hrldcpr/hungarian/master/asp.cpp
// which in turn is adapted from Dominic Battre's implementation of the Hungarian algorithm by hbc@mit.edu
// original license is MIT
object KuhnMunkres {
  final type Cost = Double
  final val INF   = 10000.0   // XXX TODO --- this doesn't look good

  def asp(size: Int, arr: Array[Array[Double]], col_mate: Array[Int], row_mate: Array[Int]): Unit = {
    val m = size
    val n = size
    var k: Int = 0
    var l: Int = 0
    var j: Int = 0
    var s: Cost = 0.0

    var t: Int = 0
    var q: Int = 0

    var unmatched: Int = 0

    util.Arrays.fill(col_mate, 0, n, 0)
    util.Arrays.fill(row_mate, 0, m, 0)

    val parent_row  = new Array[Int ](n)
    val unchosen_row= new Array[Int ](m)
    val row_dec     = new Array[Cost](m)
    val col_inc     = new Array[Cost](n)
    val slack       = new Array[Cost](n)
    val slack_row   = new Array[Int ](n)

    // Do heuristic
    l = 0
    while (l < n) {
      // find s, the minimum element in column n
      s = arr(0)(l)
      k = 1
      while (k < n) {
        if (arr(k)(l) < s) s = arr(k)(l)
        k += 1
      }
      // if s is not zero, subtract s from the column
      if (s != 0) {
        k = 0
        while (k < n) {
          arr(k)(l) -= s
          k += 1
        }
      }
      l += 1
    }

    // initialize
    l = 0
    while (l < n) {
      row_mate(l)   = -1
      parent_row(l) = -1
      col_inc(l)    = 0.0
      slack(l)      = INF
      l += 1
    }

    t = 0
    k = 0
    while (k < m) {
      val row = arr(k)
      s = row(0)
      l = 1
      while (l < n) {
        val c = row(l)
        if (c < s) s = c
        l += 1
      }
      row_dec(k) = s
      l = 0
      while (l < n && !((s == row(l)) && (row_mate(l) < 0))) l += 1
      if (l < n) {
        col_mate(k) = l
        row_mate(l) = k
      } else {
        col_mate(k) = -1
        unchosen_row(t) = k; t += 1
      }
      k += 1
    }

    if (t > 0) {
      unmatched = t
      while ({
        q = 0
        while (true) {
          while (q < t) {
            k = unchosen_row(q)
            s = row_dec(k)
            l = 0
            while (l < n) {
              if (slack(l) != 0) {
                val del: Cost = arr(k)(l) - s + col_inc(l)
                if (del < slack(l)) {
                  if (del == 0) {
                    if (row_mate(l) < 0) ??? // goto breakthru;
                    slack(l) = 0.0
                    parent_row(l) = k
                    unchosen_row(t) = row_mate(l)
                    t += 1
                  } else {
                    slack(l) = del
                    slack_row(l) = k
                  }
                }
              }
              l += 1
            }
            q += 1
          }

          s = INF
          l = 0

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
          while (l < n) {
            if (slack(l) != 0) {
              slack(l) -= s
              if (slack(l) == 0) {
                k = slack_row(l)
                if (row_mate(l) < 0) {
                  j = l + 1
                  while (j < n) {
                    if (slack(j) == 0) col_inc(j) += s
                    j += 1
                  }
                  ??? // goto breakthru;
                } else {
                  parent_row(l) = k
                  unchosen_row(t) = row_mate(l)
                  t += 1
                }
              }
            } else {
              col_inc(l) += s
            }
            l += 1
          }
        }

        ??? // breakthru:
        while ({
          j = col_mate(k)
          col_mate(k) = l
          row_mate(l) = k
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
          slack(l) = INF
          l += 1
        }
        k = 0
        while (k < m) {
          if (col_mate(k) < 0) {
            unchosen_row(t) = k; t += 1
          }
          k += 1
        }
      }
    }

    k = 0
    while (k < m) {
      l = 0
      while (l < n) {
        arr(k)(l) = arr(k)(l) - row_dec(k) + col_inc(l)
        l += 1
      }
      k += 1
    }
  }
}
