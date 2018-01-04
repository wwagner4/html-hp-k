package net.entelijan.kimi

import net.entelijan.kimi.Model._

object Photocredit {

  def credit(lang: Lang, name: String): Option[String] = creditForName(name) match {
    case Some(txt) => lang match {
      case Eng => Some("Photo: %s" format txt)
      case Ger => Some("Foto: %s" format txt)
    }
    case None => None
  }

  private val td = "KorneliusTarmann_Design"
  private val j = "Susi Jirkuff, Wild Wood, 2013"

  private def creditForName(name: String): Option[String] = name match {
    case "dogdays_EN.jpg"        => Some(td)
    case "paradiesglaube_DE.jpg" => Some(td)
    case "paradiesglaube_EN.jpg" => Some(td)
    case "jirkuff_START.jpg"     => Some(j)
    case _                       => None
  }

}