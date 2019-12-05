package nikolaj.westergaard

import scala.collection.mutable
import scala.util.{Success, Try}

object Dec05 {
  val mem: mutable.HashMap[Int, Int] = mutable.HashMap()

  def process(ins: List[Int], testinst: List[Int], pos: Int = 0): List[Int] = ins(pos) match {
    case p if p > 99 =>
      val s = p.abs.formatted("%05d")
      s.substring(3) match {

        case "01" =>
          val v1 = if (s(2) == '0') ins(ins(pos + 1)) else ins(pos + 1)
          val v2 = if (s(1) == '0') ins(ins(pos + 2)) else ins(pos + 2)
          process(ins.updated(ins(pos + 3), v1 + v2), testinst, pos + 4)

        case "02" =>
          val v1 = if (s(2) == '0') ins(ins(pos + 1)) else ins(pos + 1)
          val v2 = if (s(1) == '0') ins(ins(pos + 2)) else ins(pos + 2)
          process(ins.updated(ins(pos + 3), v1 * v2), testinst, pos + 4)

        case "04" =>
          println(f"out: ${ins(pos + 1)}")
          process(ins, testinst, pos + 2)

        case err =>
          println(s"err ${err}")
          process(ins, testinst, pos + 1)

      }
    case 1 =>
      process(ins.updated(ins(pos + 3), ins(ins(pos + 1)) + ins(ins(pos + 2))), testinst, pos + 4)

    case 2 =>
      process(ins.updated(ins(pos + 3), ins(ins(pos + 1)) * ins(ins(pos + 2))), testinst, pos + 4)

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