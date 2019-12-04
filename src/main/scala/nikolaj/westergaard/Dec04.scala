package nikolaj.westergaard

object Dec04 {

  def check(onlyPairs: Boolean = true)(p: String): Boolean = {
    var last = p.head.toInt
    var matches = false
    var matchesCount = 0
    p.substring(1).foreach(c => {
      if (c.toInt < last)
        return false // Digit lower, abandon ship, no need to continue!
      if (c.toInt == last) {
        matchesCount += 1
      } else {
        if (matchesCount == 1 || (!onlyPairs && matchesCount > 0))
          matches = true
        matchesCount = 0
      }
      last = c.toInt
    })
    if (matchesCount == 1 || (!onlyPairs && matchesCount > 0))
      matches = true
    matches
  }

  def getPasswords(range: Range, onlyPairs: Boolean = false): Seq[String] =
    range.map(_.toString).filter(check(onlyPairs))
}

object Dec04_01 extends App {
  println(s"Passwords ${Dec04.getPasswords(156218 to 652527).length}")
}

object Dec04_02 extends App {
  println(s"Passwords ${Dec04.getPasswords(156218 to 652527).length}")
}