package nikolaj.westergaard

object Dec10 {
  def loadMap(s: String): List[(Int, Int)] =
    s.split("\n")
      .zipWithIndex
      .flatMap(l => l._1.toList.zipWithIndex.flatMap(a => if (a._1 == '#') Some(a._2, l._2) else None))
      .toList

  def getBestLineOfSight(map: List[(Int, Int)]): ((Int, Int), Int) =
    map.map(a => (a, map.filter(_ != a).map(p => Math.atan2(p._1 - a._1, p._2 - a._2)).distinct.length)).maxBy(_._2)

  def getDestructionMap(map: List[(Int, Int)], p: (Int, Int)): Seq[((Int, Int), Double, Int)] =
    map.filter(_ != p)
      .map(a => (a, Math.toDegrees(Math.atan2(a._1 - p._1, p._2 - a._2)), (a._1 - p._1).abs + (a._2 - p._2).abs))
      .map(a => a.copy(_2 = if (a._2 < 0) 360 + a._2 else a._2))
      .distinctBy(_._2)
      .sortBy(_._2)
}

object Dec10_01 extends App {
  val map = Dec10.loadMap(AoCCommon.load("dec10.txt").mkString("\n"))
  println(Dec10.getBestLineOfSight(map))
}

object Dec10_02 extends App {
  val map = Dec10.loadMap(AoCCommon.load("dec10.txt").mkString("\n"))
  val dm = Dec10.getDestructionMap(map, (19, 14))
  println(dm(199))
}