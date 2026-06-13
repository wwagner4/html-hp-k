package net.entelijan.kimi.renderer

import net.entelijan.kimi.Model._

object SpecialProjectPage {

  // TODO Remove Special Project
  // private val pages: Map[String, (Lang) => String] = Map("jirkuff" -> jirkuff)
  private val pages: Map[String, (Lang) => String] = Map()

  def isSpecialPage(id: String): Boolean = pages.contains(id)

  def specialPage(id: String, lang: Lang): String = pages(id)(lang)

}
