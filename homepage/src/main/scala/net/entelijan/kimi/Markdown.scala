package net.entelijan.kimi

import scala.io.Codec
import laika.api._
import laika.parse.markdown.Markdown
import laika.render.HTML
import scala.language.postfixOps

object Md {
  def transf(in: String): String = {

    implicit val codec: Codec = Codec.UTF8
    val internal = Parse as Markdown fromString in
    Render as HTML from internal toString
  }

}