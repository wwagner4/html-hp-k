package net.entelijan.kimi

object KimiUtil {

  def uqual(in: String): String = {
    val R = """(.*)\(.*\)""".r
    in match {
      case R(a) => a.trim()
      case _    => in
    }
  }

}