package nikolaj.westergaard

import java.util

import scala.collection.convert.ImplicitConversionsToScala.`list asScalaBuffer`

object Dec08 {
  def getLayers(p: List[Char], w: Int, h: Int): List[List[Char]] = {
    var l: List[util.LinkedList[Char]] = List()
    p.zipWithIndex.foreach(c => {
      if (c._2 % (w * h) == 0) l = l :+ new util.LinkedList()
      l.last.add(c._1)
    })
    l.map(_.toList)
  }

  def getImage(layers: List[List[Char]], w: Int, h: Int): List[Char] = {
    var pixels: List[Char] = 0.until(w * h).toList.asInstanceOf[List[Char]]
    layers.foreach(l => {
      l.zipWithIndex.foreach(p => {
        if (p._1 == '0' || p._1 == '1') pixels = pixels.updated(p._2, p._1)
      })
    })
    pixels
  }
}

object Dec08_01 extends App {
  val input = AoCCommon.load("dec08.txt").next().toList
  val layers = Dec08.getLayers(input, 25, 6)
    .minBy(_.foldLeft(0)((m, c) => if (c == '0') m + 1 else m))
    .foldLeft((0, 0))((c, m) => (if (m == '1') c._1 + 1 else c._1, if (m == '2') c._2 + 1 else c._2))
  println(layers._1 * layers._2)
}

object Dec08_02 extends App {
  val input = AoCCommon.load("dec08.txt").next().toList
  val layers = Dec08.getLayers(input, 25, 6).reverse
  val pixels = Dec08.getImage(layers, 25, 6)
  0 to 5 foreach(i => {
    println(pixels.map(p => if (p == '0') ' ' else '#' ).mkString.substring(i * 25, i * 25 + 25))
  })
}