package net.entelijan.kimi

object ReportFormatterTxt {

  import Model._

  def format(r: Report): Unit = println(formatRec(r, 0))

  private def formatRec(rep: ReportBody, depth: Int): String = {
    val sb = new StringBuilder()
    rep match {
      case r: Report =>
        if (r.heading.isDefined) {
          sb.append(indentStr(depth))
          sb.append(r.heading.get)
          sb.append('\n')
        }
        r.body.foreach(x => {
          sb.append(formatRec(x, depth + 1))
        })
      case r: ReportLines =>
        r.lines.foreach(l => {
          sb.append(indentStr(depth))
          sb.append(l)
          sb.append('\n')
        })
    }
    sb.toString
  }

  private def indentStr(depth: Int): String = (0 until depth).map(_ => "\t").mkString("")

}