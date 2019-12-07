package nikolaj.westergaard

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import scala.concurrent.duration.DurationLong
import scala.concurrent.{Await, Future}

object Dec07 {
  def process(ins: List[Int], testinst: BlockingQueue[Int], pos: Int = 0, m1: Int = 0, m2: Int = 0, out: BlockingQueue[Int], no: Int = 0, oi: Option[Int] = None): (List[Int], BlockingQueue[Int], Boolean) = {
    if (pos < 0)
      return (ins, out, true)
    lazy val v1 = if (m1 == 0) ins(ins(pos + 1)) else ins(pos + 1)
    lazy val v2 = if (m2 == 0) ins(ins(pos + 2)) else ins(pos + 2)
    oi.getOrElse(ins(pos)) match {
      case p if p > 99 =>
        val s = p.abs.formatted("%05d")
        process(ins, testinst, pos, s(2).asDigit, s(1).asDigit, out, no = no, Some(s.substring(3).toInt))
      case 1 => process(ins.updated(ins(pos + 3), v1 + v2), testinst, pos + 4, out = out, no = no)
      case 2 => process(ins.updated(ins(pos + 3), v1 * v2), testinst, pos + 4, out = out, no = no)
      case 3 =>
        val inst = testinst.take()
        process(ins.updated(ins(pos + 1), inst), testinst, pos + 2, out = out, no = no)
      case 4 =>
        // Output handler to continue
        out.add(v1)
        process(ins, testinst, pos + 2, m1, m2, out, no = no)
      case 5 => process(ins, testinst, if (v1 != 0) v2 else pos + 3, out = out, no = no)
      case 6 => process(ins, testinst, if (v1 == 0) v2 else pos + 3, out = out, no = no)
      case 7 => process(ins.updated(ins(pos + 3), if (v1 < v2) 1 else 0), testinst, pos + 4, out = out, no = no)
      case 8 => process(ins.updated(ins(pos + 3), if (v1 == v2) 1 else 0), testinst, pos + 4, out = out, no = no)
      case 99 => (ins, out, true)
    }
  }

  def runChain(input: List[Int], p: List[Int]): List[Int] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val queues = 0.to(4).map(i => {
      val q = new LinkedBlockingQueue[Int]()
      q.add(p(i))
      if (i == 0) q.add(0)
      q
    })

    val f = 0.to(4).map(i =>
      Future(process(input, queues(i), out = queues((i + 1) % 5), no = i))
    )(4)

    Await.result(f, 30.seconds)._2.toArray.toList.asInstanceOf[List[Int]]
  }
}

object Dec07_01 extends App {
  val pars = 0.to(4)
    .flatMap(i => 0.to(4).map(in => (i, in)))
    .flatMap(i => 0.to(4).map(in => (i._1, i._2, in)))
    .flatMap(i => 0.to(4).map(in => (i._1, i._2, i._3, in)))
    .flatMap(i => 0.to(4).map(in => (i._1, i._2, i._3, i._4, in)))
    .map(p => List(p._1, p._2, p._3, p._4, p._5))
    .filter(_.distinct.length == 5)

  val input = AoCCommon.loadIntList("dec07.txt")
  val fin = pars.map(p => {
    Dec07.runChain(input, p)
  }).toList.flatten.max
  println(fin)
}

object Dec07_02 extends App {
  val pars = 5.to(9)
    .flatMap(i => 5.to(9).map(in => (i, in)))
    .flatMap(i => 5.to(9).map(in => (i._1, i._2, in)))
    .flatMap(i => 5.to(9).map(in => (i._1, i._2, i._3, in)))
    .flatMap(i => 5.to(9).map(in => (i._1, i._2, i._3, i._4, in)))
    .map(p => List(p._1, p._2, p._3, p._4, p._5))
    .filter(_.distinct.length == 5)

  val input = AoCCommon.loadIntList("dec07.txt")
  val fin = pars.map(p => {
    val t = Dec07.runChain(input, p)
    t
  }).toList.flatten.max
  println(fin)
}