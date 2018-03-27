package net.entelijan.kimi.renderer

import net.entelijan.kimi.HtmlTemplateEngine._
import net.entelijan.kimi.Model._
import net.entelijan.kimi.PagesConf.{ProjectPage, StartPage}
import net.entelijan.kimi.{Md, Model, PagesConf, Photocredit}


class MobileRenderer01 extends DefaultRenderer {

  private val headlinePortrait = MultiLangGeneric(
    s"""
       |<div class="headline">
       |  <span class="headline1">KIMI LUM</span>
       |</div>
       |<div class="headlineB">
       |  <span class="headline2">ÜBERSETZERIN</span>
       |  <span class="headline3">aus dem Deutschen ins Englische</span>
       |</div>
       """.stripMargin,
    s"""
       |<div class="headline">
       |  <span class="headline1">KIMI LUM</span>
       |</div>
       |<div class="headlineB">
       |  <span class="headline2">TRANSLATOR</span>
       |  <span class="headline3">from German to English</span>
       |</div>
       """.stripMargin)

  private val headlineLandscape = MultiLangGeneric(
    s"""
       |<div class="headline">
       |  <span class="headline1">KIMI LUM</span>
       |  <span class="headline2">ÜBERSETZERIN</span>
       |  <span class="headline3">aus dem Deutschen ins Englische</span>
       |</div>
       |</div>
       """.stripMargin,
    s"""
       |<div class="headline">
       |  <span class="headline1">KIMI LUM</span>
       |  <span class="headline2">TRANSLATOR</span>
       |  <span class="headline3">from German to English</span>
       |</div>
       |</div>
       """.stripMargin)


  override def rendBody(currentPage: Model.Page, pages: List[Model.Page], lang: Model.Lang, languages: List[Model.Lang], pageContent: Model.DeviceData[String]): Model.DeviceData[String] = {
    pageContent.device match {
      case DV_Browser => super.rendBody(currentPage, pages, lang, languages, pageContent)
      case DV_MobilePortrait => rendMobileBody(currentPage, pages, lang, languages, pageContent, headlinePortrait)
      case DV_MobileLandscape => rendMobileBody(currentPage, pages, lang, languages, pageContent, headlineLandscape)
    }
  }

  def rendMobileBody(currentPage: Page, pages: List[Page], lang: Lang, languages: List[Lang], pageContent: DeviceData[String], headline: MultiLang[String]): DeviceData[String] = {

    def menuItemClass(loc: Location, active: Boolean): String =
      loc match {
        case LOC_First => "menuItemF"
        case LOC_Middle => "menuItemM"
        case LOC_Last => "menuItemM"
      }


    def menuInfo(current: Page, menuPages: List[MenuPage], lang: Lang): String = {
      val menueRendered: Seq[String] = menuPages.map(menuPage => {
        val active = current.id == menuPage.page.id
        val clazz = menuItemClass(menuPage.location, active)
        val idAttr = menuPage.id match {
          case None => ""
          case Some(id) => s"""id="$id" """
        }
        val style = if (current.pageType != PT_Project || menuPage.id.isEmpty || menuPage.id.get != "prjAlphaPage") ""
        else
          """ style="padding-bottom: 600px" """

        val pageName: MultiLang[PageName] =
          if (menuPage.page.id.startsWith("prj"))
            MultiLangGeneric[PageName](
              PageName(None, "Projekte"),
              PageName(None, "Projects")
            )
          else if (menuPage.page.id.startsWith("bio"))
            MultiLangGeneric[PageName](
              PageName(None, "BIO / KONTAKT"),
              PageName(None, menuPage.page.name.eng.name)
            )
          else menuPage.page.name

        if (menuPage.page != current)
          s"""<div $idAttr $style class="$clazz"><a href="${fileName(menuPage.page, lang)}">${pageNameHtml(pageName.value(lang))}</a></div>"""
        else
          s"""<div $idAttr $style class="$clazz">${pageNameHtml(pageName.value(lang))}</div>"""
      })
      val menuRenderedString = menueRendered.mkString("")
      val m = menuRenderedString + menuLang(current, lang)
      s"""<div id="infoMenu"><div class="menuItemFill"></div>$m</div>"""
    }

    def menuLang(current: Page, lang: Lang): String = {
      val name = lang match {
        case Eng => "DE"
        case Ger => "EN"
      }
      def otherLang(l: Lang): Lang = l match {
        case Ger => Eng
        case Eng => Ger
      }
      s"""<div id="lang" class="menuItemL" style="padding-left : 4px">
         |<a href="${fileName(current, otherLang(lang))}">
         |<span class="infoMenuItemText">$name</span>
         |</a>
         |</div>""".stripMargin
    }

    val contentId = currentPage.pageType match {
      case PT_Start => "contentStart"
      case PT_Project => "mobileContentPrj"
      case _ => "mobileContent"
    }

    def menuPagesMobile(pages: List[Page]): List[MenuPage] = {
      def convertToMenuPages(pages: List[Page]): List[MenuPage] = {
        val locs = locations(pages.size)
        pages.zip(locs) map { case (page, loc) => MenuPage(page, loc, page.menuId) }
      }

      val relPages = pages
        .filter { page =>
          page.pageType == PT_Start || page.pageType == PT_ProjectOverview || page.pageType == PT_Info
        }
        .filter { page =>
          page.id != "prjAuthPage" && page.id != "prjAlphaPage"
        }
        .sortBy(_.menuSortOrder)
      convertToMenuPages(relPages)
    }

    def gerTxt(pContent: String) =
      s"""${headline.ger}
         |${menuInfo(currentPage, menuPagesMobile(pages), lang)}
         |<div id="$contentId">
         |$pContent
         |</div>
         |""".stripMargin

    def engTxt(pContent: String) =
      s"""${headline.eng}
         |${menuInfo(currentPage, menuPagesMobile(pages), lang)}
         |<div id="$contentId">
         |$pContent
         |</div>
         |""".stripMargin

    new MultiLangGeneric[DeviceData[String]](
      pageContent.copy(data = gerTxt(pageContent.data)),
      pageContent.copy(data = engTxt(pageContent.data))
    ).value(lang)

  }

  override def rendPageStart(page: StartPage, lang: Lang, device: Device): String = {
    device match {
      case DV_Browser => super.rendPageStart(page, lang, device)
      case DV_MobilePortrait => rendMobilePageStart(page, lang, device)
      case DV_MobileLandscape => rendMobilePageStart(page, lang, device)
    }
  }

  def rendMobilePageStart(page: StartPage, lang: Lang, device: Device): String = {

    case class ImagePlacement(prj: Project, alignment: TxtAlignment, xoff: Int, yoff: Int, imgName: String)

    def alph(from: Char, to: Char, lang: Lang) = (from to to).map(c => createChar(c, lang)).mkString("\n")

    def alph1(chars: List[Char], lang: Lang) = chars.map(c => createChar(c, lang)).mkString("\n")

    def createChar(c: Char, lang: Lang): String = {
      val prjs = page.projects.filter(p => {
        val ao = p.title.value(lang).homepageAlph
        ao.isDefined && ao.get.toUpper == c.toUpper
      })
      if (prjs.isEmpty) {
        s"""<span class="startChar">$c</span>"""
      } else {
        val p = prjs(0)
        s"""<div class="startChar" id="char_${p.id}"><a href="${fileName(p, lang)}">$c</a></div>"""
      }
    }

    s"""<div id="startAphaBlock">
       |<div>${alph('A', 'E', lang)}</div>
       |<div>${alph('F', 'J', lang)}</div>
       |<div>${alph('K', 'O', lang)}</div>
       |<div>${alph('P', 'T', lang)}</div>
       |<div>${alph1(List('U', 'V', 'W', 'X', 'Z'), lang)}</div>
       |</div>
       |<div id="startAlphaBlockBack"></div>
       |""".stripMargin
  }


  override def rendPageProject(page: PagesConf.ProjectPage, lang: Lang, device: Device): String = {
    device match {
      case DV_Browser => super.rendPageProject(page, lang, device)
      case DV_MobilePortrait => rendMobilePageProject(page, lang, device, "85%")
      case DV_MobileLandscape => rendMobilePageProject(page, lang, device, "65%")
    }
  }

  def rendMobilePageProject(page: ProjectPage, lang: Lang, device: Device, imageWidth: String): String = {

    val prj = page.prj

    val isbn = if (prj.isbn.isDefined)
      s"""${prj.isbn.get}"""
    else ""

    def artist(lang: Lang): String = ArtistRolesUtil.format(prj.artist, lang).mkString("</br>")

    def beitrag(lang: Lang): String = prj.contrib match {
      case None => ""
      case Some(a) => "| %s" format a.value(lang)
    }

    def yearString: String = prj.year match {
      case None => " "
      case Some(y) => ", " + y.trim()
    }

    def comp(lang: Lang) = if (prj.company.isDefined)
      s"""${prj.company.get.typ.name.value(lang)}: ${prj.company.get.name.trim}"""
    else ""

    def credit(lang: Lang) = Photocredit.credit(lang, prj.images.imageInfo(lang, ILProject).name) match {
      case Some(c) => c.trim()
      case None => ""
    }

    def details(lang: Lang): String = prj.projectDetails match {
      case None =>
        val mdt = isbn match {
          case "" =>
            s"""${artist(lang)}
               |
               |${comp(lang)}$yearString
               |${credit(lang)}
               |""".stripMargin.trim()
          case _ =>
            s"""${artist(lang)}
               |
               |${comp(lang)}$yearString
               |$isbn
               |${credit(lang)}
               |""".stripMargin.trim()
        }
        Md.transf(mdt.trim())
      case Some(txt) =>
        val mdt = txt.value(lang)
        val re = Md.transf(mdt)
        re
    }

    def subtitleHtml(lang: Lang): String = {

      prj.subTitle match {
        case None => ""
        case Some(st) => s"""<div class="prjSubtitle">${st.value(lang)}</div>"""
      }

    }

    def contentDefault(lang: Lang): String =
      s"""<div class="prjBody">
         |<div class="prjTitle">${prj.title.value(lang).name.getUqual.toUpperCase()} ${beitrag(lang)}</div>
         |${subtitleHtml(lang)}
         |<div class="prjText">
         |${details(lang)}
         |</div>
         |</div>
         |<img class="prjImg" src="images/${prj.images.imageInfo(lang, ILProject).name}" width="$imageWidth" />
         |""".stripMargin

    contentDefault(lang)
  }


}
