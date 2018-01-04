package net.entelijan.kimi

import net.entelijan.kimi.Model._

object SpecialProjectPage {

  private val pages: Map[String, (Lang) => String] = Map("jirkuff" -> jirkuff)
  
  
  def isSpecialPage(id: String): Boolean = pages.contains(id)
  
  def specialPage(id: String, lang: Lang): String = pages(id)(lang)
  
  private def jirkuff(lang: Lang): String = lang match {
    case Eng => """
<div style="width: 310px;">
<img class="prjImg" src="images/jirkuff_PRJ_2.jpg"/>
<img style="margin-top: 15px;" class="prjImg" src="images/jirkuff_PRJ_1.jpg"/>
</div>  
<div class="prjBody">
  <div class="prjTitle">BOYZ IN THE WOOD | Short film</div>
  <div style="padding-left:535px" class="prjText">
    <p>Artist: Susi Jirkuff</p>
    <p>Installation view: Wild Wood, Secession 2013
    <br/>Photos: Wolfgang Thaler</p>
  </div>
</div>
      """
    case Ger => """
<div style="width: 310px;">
<img class="prjImg" src="images/jirkuff_PRJ_2.jpg"/>
<img style="margin-top: 15px;" class="prjImg" src="images/jirkuff_PRJ_1.jpg"/>
</div>  
<div class="prjBody">
  <div class="prjTitle">BOYZ IN THE WOOD | Kurzfilm</div>
  <div style="padding-left:535px" class="prjText">
    <p>Künstlerin: Susi Jirkuff</p>
    <p>Installationsansicht: Wild Wood, Secession 2013
    <br/>Fotos: Wolfgang Thaler</p>
  </div>
</div>
      """
  }
  
}