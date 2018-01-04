package net.entelijan.kimi

object MarkdownTryout extends App {

  def md(in: String): String = {
    import laika.api._
    import laika.parse.markdown.Markdown
    import laika.render.HTML

    import scala.io.Codec
    import scala.language.postfixOps
    
    implicit val codec: Codec = Codec.UTF8
    val internal = Parse as Markdown fromString txt
    Render as HTML from internal toString
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