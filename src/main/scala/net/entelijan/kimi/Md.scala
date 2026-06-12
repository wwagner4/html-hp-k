package net.entelijan.kimi

import laika.api._
import laika.format.Markdown
import laika.format.HTML

object Md {
  def transf(in: String): String = {

    val result = Transformer
      .from(Markdown)
      .to(HTML)
      .build
      .transform(in)

    result match {
      case Right(html) =>
        html.toString()
      case Left(error) =>
        sys.error(s"Error parsing markdown : ${error.message}")
    }
  }

}
