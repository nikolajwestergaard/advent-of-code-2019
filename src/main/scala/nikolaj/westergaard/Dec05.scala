package nikolaj.westergaard

object Dec05 {
  def process(ins: List[Int], testinst: List[Int], pos: Int = 0, m1: Int = 0, m2: Int = 0, m3: Int = 0): List[Int] = {
    lazy val v1 = if (m1 == 0) ins(ins(pos + 1)) else ins(pos + 1)
    lazy val v2 = if (m2 == 0) ins(ins(pos + 2)) else ins(pos + 2)
    ins(pos) match {
      case p if p > 99 =>
        val s = p.abs.formatted("%05d")
        process(ins.updated(pos, s.substring(3).toInt), testinst, pos, s(2).asDigit, s(1).asDigit)
      case 1 => process(ins.updated(ins(pos + 3), v1 + v2), testinst, pos + 4)
      case 2 => process(ins.updated(ins(pos + 3), v1 * v2), testinst, pos + 4)
      case 3 => process(ins.updated(ins(pos + 1), testinst.head), testinst.drop(1), pos + 2)
      case 4 =>
        println(f"Code: ${ins(ins(pos + 1))}")
        process(ins, testinst, pos + 2)
      case 5 => process(ins, testinst, if (v1 != 0) v2 else pos + 3)
      case 6 => process(ins, testinst, if (v1 == 0) v2 else pos + 3)
      case 7 => process(ins.updated(ins(pos + 3), if (v1 < v2) 1 else 0), testinst, pos + 4)
      case 8 => process(ins.updated(ins(pos + 3), if (v1 == v2) 1 else 0), testinst, pos + 4)
      case 99 => ins
    }
  }
}

object Dec05_01 extends App {
  Dec05.process(AoCCommon.loadIntList("dec05.txt"), List(1))
}

object Dec05_02 extends App {
  Dec05.process(AoCCommon.loadIntList("dec05.txt"), List(5))
}