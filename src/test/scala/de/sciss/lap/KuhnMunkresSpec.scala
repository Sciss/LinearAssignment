package de.sciss.lap

import org.scalatest.{FlatSpec, Matchers}

class KuhnMunkresSpec extends FlatSpec with Matchers {
  // cf. https://de.wikipedia.org/wiki/Ungarische_Methode
  "example-1" should "work" in {
    val rowLb = Seq("A", "B", "C", "D")
    val colLb = Seq("E", "K", "P", "Z")
    val prio = Array(
      Array[Double](1, 1, 1, 2),
      Array[Double](3, 2, 4, 1),
      Array[Double](4, 4, 2, 4),
      Array[Double](2, 3, 3, 3)
    )
    val rowM = new Array[Int](4)
    val colM = new Array[Int](4)
    KuhnMunkres(prio, colM, rowM)
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
      Array[Double](0, 0, 0, 0),
      Array[Double](0, 1, 3, 3),
      Array[Double](0, 5, 5, 9),
      Array[Double](0, 1, 3, 7)
    )
    val rowM = new Array[Int](4)
    val colM = new Array[Int](4)
    KuhnMunkres(prio, colM, rowM)
//    println(rowM.mkString("row: [", ", ", "]"))
//    println(colM.mkString("col: [", ", ", "]"))

    assert((rowM sameElements Array(3, 2, 0, 1)) || (rowM sameElements Array(2, 3, 0, 1)))
  }
}