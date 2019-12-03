package nikolaj.westergaard

object Dec03 {
  
  def getPath(plot: Array[String]): List[(Int, Int, Int)] = plot.foldLeft(List((0, 0, 0)))((positions, inst) => {
    val last = positions.last
    val step = last._3
    val walk = 1.to(inst.substring(1).toInt)
    val add = inst(0) match {
      case 'U' => walk.map(p => (last._1, last._2 + p, step + p))
      case 'D' => walk.map(p => (last._1, last._2 - p, step + p))
      case 'R' => walk.map(p => (last._1 + p, last._2, step + p))
      case 'L' => walk.map(p => (last._1 - p, last._2, step + p))
      case _ => List()
    }
    positions.appendedAll(add)
  }).drop(1).distinctBy(p => (p._1, p._2))

  def loadPaths: (Array[String], Array[String]) = {
    val lines = AoCCommon.load("dec03.txt")
    (lines.next().split(","), lines.next().split(","))
  }

  def pathIntersections(p1: Array[String], p2: Array[String]): Map[(Int, Int), List[(Int, Int, Int)]] =
    Dec03.getPath(p1).appendedAll(Dec03.getPath(p2))
      .groupBy(p => (p._1, p._2))
      .filter(_._2.length > 1)

}

object Dec03_01 extends App {
  val (p1, p2) = Dec03.loadPaths
  val intersections = Dec03.pathIntersections(p1, p2)
    .map(p => p._2.head._1.abs + p._2.head._2.abs) // Get Manhattan distance
  print(intersections.min)
}

object Dec03_02 extends App {
  val (p1, p2) = Dec03.loadPaths
  val intersections = Dec03.pathIntersections(p1, p2)
    .map(p => p._2.map(_._3).sum) // Get sum of path length
  println(intersections.min)
}