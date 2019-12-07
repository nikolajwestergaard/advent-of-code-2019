package nikolaj.westergaard

import scala.collection.immutable.HashMap


object Dec06 {
  def createGraph(list: List[(String, String)], graph: HashMap[String, List[String]] = HashMap()): HashMap[String, List[String]] =
    list.foldLeft(graph)((g, c) => g.updated(c._1, g.getOrElse(c._1, List()) :+ c._2))

  def getRoots(list: List[(String, String)]): List[String] = {
    list.map(_._1).distinct.diff(list.map(_._2).distinct)
  }

  def getTotalOrbits(graph: HashMap[String, List[String]], start: String, depth: Int = 0): Int = {
    var total = 0
    total = total + depth
    graph.get(start).foreach(_.foreach { p =>
      total += getTotalOrbits(graph, p, depth + 1)
    })
    total
  }

  def getParent(name: String, parents: List[(String, String)]): Option[String] =
    parents.find(p => p._2 == name).map(_._1)

  def travel(from: String, to: String, omit: Option[String], graph: HashMap[String, List[String]], parents: List[(String, String)], distance: Int = 0): Option[Int] = {
    val res = graph.getOrElse(from, List()).flatMap(p => {
      if (p == to)
        return Some(distance - 1)
      if (!omit.contains(p))
        travel(p, to, None, graph, parents, distance + 1)
      else None
    })
    if (res.nonEmpty)
      return res.headOption
    if (omit.isDefined)
      getParent(from, parents)
        .flatMap(p => travel(p, to, Some(from), graph, parents, distance + 1))
    else None
  }
}

object Dec06_01 extends App {
  val data = AoCCommon.load("dec06.txt").map(_.trim.split(')')).map(l => (l(0), l(1))).toList
  val graph = Dec06.createGraph(data)
  println(Dec06.getTotalOrbits(graph, "COM"))
}

object Dec06_02 extends App {
  val data = AoCCommon.load("dec06.txt").map(_.trim.split(')')).map(l => (l(0), l(1))).toList
  val graph = Dec06.createGraph(data)
  println(Dec06.travel("YOU", "SAN", Some("YOU"), graph, data))

}
