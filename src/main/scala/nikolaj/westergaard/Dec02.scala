package nikolaj.westergaard

import scala.util.{Success, Try}

object Computer {
  def process(ins: List[Int], pos: Int = 0): List[Int] = ins(pos) match {
    case 1 =>
      process(ins.updated(ins(pos + 3), ins(ins(pos + 1)) + ins(ins(pos + 2))), pos + 4)
    case 2 =>
      process(ins.updated(ins(pos + 3), ins(ins(pos + 1)) * ins(ins(pos + 2))), pos + 4)
    case 99 =>
      ins
  }
}

object Dec02_01 extends App {
  println(Computer.process(AoCCommon.loadIntList("dec02.txt").updated(1, 12).updated(2, 2)).head)
}

object Dec02_02 extends App {
  val baseInst = AoCCommon.loadIntList("dec02.txt")

  // Lets just brute-force it!
  0.to(100)
    .flatMap(noun => 0 to 100 map (verb => (noun, verb)))
    .find(t => {
      Try(Computer.process(baseInst.updated(1, t._1).updated(2, t._2)).head) match {
        case Success(19690720) => true
        case _ => false
      }
    }) match {
    case Some((noun, verb)) => println(s"Noun: $noun, Verb: $verb, ${100 * noun + verb}")
    case None => println("Damn")
  }
}
