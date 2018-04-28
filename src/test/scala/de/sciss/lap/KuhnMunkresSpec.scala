package de.sciss.lap

import org.scalatest.{FlatSpec, Matchers}

class KuhnMunkresSpec extends FlatSpec with Matchers {
  // cf. https://de.wikipedia.org/wiki/Ungarische_Methode
  "example-1" should "work" in {
    val rowLb = Seq("A", "B", "C", "D")
    val colLb = Seq("E", "K", "P", "Z")
    val prio = Array(
      Array[Float](1, 1, 1, 2),
      Array[Float](3, 2, 4, 1),
      Array[Float](4, 4, 2, 4),
      Array[Float](2, 3, 3, 3)
    )
    val rowM = new Array[Int](4)
    val colM = new Array[Int](4)
    KuhnMunkres(prio, rowMap = rowM, colMap = colM)
//    println(rowM.mkString("row: [", ", ", "]"))
//    println(colM.mkString("col: [", ", ", "]"))

    val expected = Map(
      "A" -> "Z", "B" -> "E", "C" -> "P", "D" -> "K"
    )

    assert(rowM.zipWithIndex.forall { case (ci, ri) =>
      expected(rowLb(ri)) == colLb(ci)
    })

    assert(colM.zipWithIndex.forall { case (ri, ci) =>
      expected(rowLb(ri)) == colLb(ci)
    })
  }

  "example-2" should "work" in {
    val prio = Array(
      Array[Float](0, 0, 0, 0),
      Array[Float](0, 1, 3, 3),
      Array[Float](0, 5, 5, 9),
      Array[Float](0, 1, 3, 7)
    )
    val rowM = new Array[Int](4)
    val colM = new Array[Int](4)
    KuhnMunkres(prio, rowMap = rowM, colMap = colM)
//    println(rowM.mkString("row: [", ", ", "]"))
//    println(colM.mkString("col: [", ", ", "]"))

    assert((rowM sameElements Array(3, 2, 0, 1)) || (rowM sameElements Array(2, 3, 0, 1)))
  }

  "large random example" should "work" in {
    def mkRandomMatrix(M: Int, N: Int = 100): (Array[Array[Float]], Array[Int]) = {
      val r = new util.Random(0L)
      val prio = Array.fill(M)(Array.fill(M)(r.nextInt(N).toFloat))
      val rowM = new Array[Int](M)
      val colM = new Array[Int](M)
      KuhnMunkres(prio, rowMap = rowM, colMap = colM)
      (prio, rowM)
    }

    val LARGE = 10000
    val (_, rowLarge) = mkRandomMatrix(LARGE)  // can't verify this using brute search, but at least check that it returns
    assert(rowLarge.sorted sameElements (0 until LARGE))

    val (m10, row10) = mkRandomMatrix(10)

    def cost(map: Seq[Int]): Double =
      map.iterator.zipWithIndex.map { case (ri, ci) =>
        m10(ri)(ci).toDouble
      } .sum

    val hCost = cost(row10)
    val brute = (0 until 10).permutations.minBy(cost)
    val bCost = cost(brute)
    assert(hCost == bCost)
    assert(row10 sameElements brute)
  }
}