package net.entelijan.kimi

object MarkdownTryout extends App {

  def md(in: String): String = {
    import laika.api.Transformer
    import laika.format.{Markdown, HTML}

    val result = Transformer
      .from(Markdown)
      .to(HTML)
      .build
      .transform(in)

    result match {
      case Right(html) =>
        html
      case Left(error) =>
        s"Error Message: ${error.message}"
    }
  }

  val txt = """
Bla BlaBla BlaBla BlaBla BlaBla BlaBla BlaBla BlaBla BlaBla Bla  
BluBlu BluBlu BluBlu BluBlu BluBlu BluBlu BluBlu 
        
Hummer Hummer Hummer Hummer Hummer Hummer Hummer 
Hammer Hammer Hammer Hammer Hammer 

Herausgeber: Bettina Götz

Künstler: Bettina Furz

Herausgeber: Bettina Grutner  
[an example](http://example.com/ "Title")
    """

  println("-----------------------------------------------------")
  println(md(txt))
  println("-----------------------------------------------------")
}
