package nikolaj.westergaard

import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}

import scala.io.Source

object AoCCommon {
  def load(file: String): Iterator[String] = Source.fromResource(file).getLines

  def loadIntList(file: String, extend: Int = 0): List[Int] = load(file).reduce(_ + _).split(",").map(_.toInt).toList :++ List.fill(extend)(0)

  def loadBigIntList(file: String, extend: Int = 0): List[BigInt] = (load(file).reduce(_ + _).split(",") :++ List.fill[String](extend)("0")).toList.map(s => BigInt(s))

  def loadBigDecimal(file: String): Iterator[BigDecimal] = AoCCommon.load(file).map(t => BigDecimal.apply(t))

  def bq[T](l: List[T] = List()): LinkedBlockingQueue[T] = {
    val q = new LinkedBlockingQueue[T]()
    l.foreach(i => q.add(i))
    q
  }
}
