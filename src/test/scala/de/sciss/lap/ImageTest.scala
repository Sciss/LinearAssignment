/*
 *  ImageTest.scala
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

import java.awt.image.BufferedImage

import de.sciss.file._
import javax.imageio.ImageIO

object ImageTest {
  case class Config(fIn: File = file("in"), fOut: File = file("out"),
                    wOut: Int = 0, hOut: Int = 0,
                    invert: Boolean = false, squareCost: Boolean = true)

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("RecordAccel") {
      opt[File]('i', "in")
        .required()
        .text ("Input image file.")
        .action { (v, c) => c.copy(fIn = v) }
      opt[File]('o', "out")
        .required()
        .text ("Output image file.")
        .action { (v, c) => c.copy(fOut = v) }
      opt[Int]('w', "width")
        .required()
        .validate { v => if (v >= 1) success else failure("Must be >= 1") }
        .text ("Output image width in pixels.")
        .action { (v, c) => c.copy(wOut = v) }
      opt[Int]('h', "height")
        .required()
        .validate { v => if (v >= 1) success else failure("Must be >= 1") }
        .text ("Output image height in pixels.")
        .action { (v, c) => c.copy(hOut = v) }
      opt[Unit]("invert")
        .text ("Invert pixel selection (drop dark pixels instead of bright).")
        .action { (_, c) => c.copy(invert = true) }
      opt[Unit]("sqrt")
        .text ("Take sqrt (Euclidean) for cost function (greater pixel scattering).")
        .action { (_, c) => c.copy(squareCost = false) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  private final case class Pixel(off: Int, rgb: Int, lum: Double)

  def run(config: Config): Unit = {
    import config._
    println("Reading input image...")
    val imgIn   = ImageIO.read(fIn)
    val wIn     = imgIn.getWidth()
    val hIn     = imgIn.getHeight()
    val inSize  = wIn  * hIn
    val numRows = wOut * hOut
    require (inSize > numRows, s"Output image ($wOut x $hOut) must be smaller than input image ($wIn x $hIn)")

    fOut.createNewFile()
    require (fOut.canWrite, s"Output file '$fOut' is not writable.")

    val all     = Array.tabulate(inSize) { i =>
      val x   = i % wIn
      val y   = i / wIn
      val rgb = imgIn.getRGB(x, y)
      val r   = (rgb >> 16) & 0xFF
      val g   = (rgb >>  8) & 0xFF
      val b   =  rgb        & 0xFF
      val lum = 0.2126 * r + 0.7152 * g + 0.0722 * b
      Pixel(i, rgb, lum)
    }

    val sorted  = all.sortBy(_.lum)
    val sel     = if (invert) sorted.takeRight(numRows) else sorted.take(numRows)
    val sx      = wOut.toFloat / wIn
    val sy      = hOut.toFloat / hIn

    val matrix  = sel.iterator.map { pix =>
      val x1 = (pix.off % wIn) * sx
      val y1 = (pix.off / wIn) * sy
      val c = Array.tabulate(numRows) { i =>
        val x2  = i % wOut
        val y2  = i / wOut
        val dx = x2 - x1
        val dy = y2 - y1
        if (squareCost)
          dx * dx + dy * dy
        else
          math.sqrt(dx * dx + dy * dy).toFloat
      }
      //      println(c.mkString("[", ", ", "]"))
      c
    } .toArray

    val colMap = new Array[Int](numRows)
    val rowMap = new Array[Int](numRows)
    println(s"Running Kuhn-Munkres (numRows = $numRows)...")
    println("_" * 100)
    val t1 = System.currentTimeMillis()
    var lastP = 0
    KuhnMunkres(matrix, colMap, rowMap, progress = { (left, of) =>
      val p = (of - left) * 100 / of
      while (lastP < p) {
        print('#')
        lastP += 1
      }
    })
    val t2 = System.currentTimeMillis()
    println(s" took ${(t2-t1)/1000}sec.")

    val imgOut = new BufferedImage(wOut, hOut, BufferedImage.TYPE_INT_RGB)
    rowMap.iterator.zipWithIndex.foreach { case (ri, ci) =>
      val rgb = sel(ri).rgb
      val x   = ci % wOut
      val y   = ci / wOut

      imgOut.setRGB(x, y, rgb)
    }

    val fmt = if (fOut.extL == "jpg") "jpg" else "png"
    println("Writing output images...")
    ImageIO.write(imgOut, fmt, fOut)

    println("Done.")
  }
}
