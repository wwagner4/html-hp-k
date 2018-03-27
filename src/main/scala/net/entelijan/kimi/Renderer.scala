package net.entelijan.kimi

import net.entelijan.kimi.Model._
import net.entelijan.kimi.PagesConf._

trait Renderer {

  final def rendPage(page: Page, pages: List[Page], lang: Lang, languages: List[Lang], devices: List[Device]): String = {

    val bodyCont = page match {
      case p: StartPage =>
        devices
          .map(d => DeviceData(d, rendPageStart(p, lang, d)))
          .map(dd => rendBody(page, pages, lang, languages, dd))
      case p: PrjAlphaPage =>
        devices
          .map(d => DeviceData(d, rendPagePrjAlpha(p, lang, d)))
          .map(dd => rendBody(page, pages, lang, languages, dd))
      case p: PrjAuthPage =>
        devices
          .map(d => DeviceData(d, rendPagePrjAuth(p, lang, d)))
          .map(dd => rendBody(page, pages, lang, languages, dd))
      case p: PrjCatPage =>
        devices
          .map(d => DeviceData(d, rendPagePrjCat(p, lang, d)))
          .map(dd => rendBody(page, pages, lang, languages, dd))
      case p: BioContactPage =>
        devices
          .map(d => DeviceData(d, rendPageBioContact(p, lang, d)))
          .map(dd => rendBody(page, pages, lang, languages, dd))
      case p: ProjectPage =>
        devices
          .map(d => DeviceData(d, rendPageProject(p, lang, d)))
          .map(dd => rendBody(page, pages, lang, languages, dd))
    }
    HtmlTemplateEngine.pageTemplate(bodyCont, page, lang)
  }

  def rendBody(currentPage: Page, pages: List[Page], lang: Lang, languages: List[Lang], pageContent: DeviceData[String]): DeviceData[String]

  def rendPageStart(page: StartPage, lang: Lang, device: Device): String

  def rendPagePrjAlpha(page: PrjAlphaPage, lang: Lang, device: Device): String

  def rendPagePrjAuth(page: PrjAuthPage, lang: Lang, device: Device): String

  def rendPagePrjCat(page: PrjCatPage, lang: Lang, device: Device): String

  def rendPageBioContact(page: BioContactPage, lang: Lang, device: Device): String

  def rendPageProject(page: ProjectPage, lang: Lang, device: Device): String

}
