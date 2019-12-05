package nikolaj.westergaard

import scala.collection.mutable
import scala.util.{Success, Try}

object Dec05 {
  val mem: mutable.HashMap[Int, Int] = mutable.HashMap()

  def process(ins: List[Int], testinst: List[Int], pos: Int = 0, m1: Int = 0, m2: Int = 0): List[Int] = ins(pos) match {
    case p if p > 99 =>
      val s = p.abs.formatted("%05d")
      val m1 = s(2).asDigit
      val m2 = s(1).asDigit
      val op = s.substring(3).toInt
      process(ins.updated(pos, op), testinst, pos, m1, m2)

    case 1 =>
      val v1 = if (m1 == 0) ins(ins(pos + 1)) else ins(pos + 1)
      val v2 = if (m2 == 0) ins(ins(pos + 2)) else ins(pos + 2)
      process(ins.updated(ins(pos + 3),  v1 + v2), testinst, pos + 4)

    case 2 =>
      val v1 = if (m1 == 0) ins(ins(pos + 1)) else ins(pos + 1)
      val v2 = if (m2 == 0) ins(ins(pos + 2)) else ins(pos + 2)
      process(ins.updated(ins(pos + 3),  v1 * v2), testinst, pos + 4)

    case 3 =>
      process(ins.updated(ins(pos + 1), testinst.head), testinst.drop(1), pos + 2)

    case 4 =>
      println(f"out: ${ins(ins(pos + 1))}")
      process(ins, testinst, pos + 2)

    case 99 =>
      ins

    case p =>
      process(ins, testinst, pos + 1)

  }
}

object Dec05_01 extends App {
  Dec05.process(AoCCommon.loadIntList("dec05.txt"), List(1))
}