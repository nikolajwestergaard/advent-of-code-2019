package nikolaj.westergaard

import scala.io.Source

object AoCCommon {
  def load(file: String): Iterator[String] = Source.fromResource(file).getLines

  def loadIntList(file: String): List[Int] = load(file).reduce(_ + _).split(",").map(_.toInt).toList

  def loadBigDecimal(file: String): Iterator[BigDecimal] = AoCCommon.load(file).map(t => BigDecimal.apply(t))
}
