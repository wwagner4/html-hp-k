package net.entelijan.kimi.renderer

import net.entelijan.kimi.HtmlTemplateEngine._
import net.entelijan.kimi.Model._
import net.entelijan.kimi.PagesConf._
import net.entelijan.kimi._

class DefaultRenderer extends Renderer {

  override def rendPagePrjAuth(page: PrjAuthPage, lang: Lang, device: Device): String = {

    def links(artist: Artist): String = {

      val prjs = page.projects.filter { p =>
        p.artist.foldRight(false)((a, b) => b || a.name == artist.name) && // What is that condition good for ??
          artist.role != AR_Undef // Ignore artists with undefined role
      }
      val prjsSort = prjs.sortBy {
        _.title.value(lang).name.alph
      }
      prjsSort.map(p => {
        val page = ProjectPage(p)
        val fname = HtmlTemplateEngine.fileName(page, lang)
        val pname = HtmlTemplateEngine.pageNameContentHtml(page.name.value(lang))
        s"""<div class="prjCatLink"><a href="$fname">$pname</a></div>"""
      }).mkString("")
    }

    val usedAuthors =
      page
        .projects.flatMap(_.artist)
        .distinct
        .sortBy(_.name.toUpperCase())

    usedAuthors.map(artist => {
      s"""<div class="prjCatHeading">${artist.name.toUpperCase()}</div>
         |${links(artist)}
         |""".stripMargin.trim
    }).mkString

  }

  override def rendPageBioContact(page: BioContactPage, lang: Lang, device: Device): String = {
    lang match {
      case Ger =>
        s"""<div class="bioText">
           |Geboren in Hawaii, USA, aufgewachsen in Kalifornien,
           |lebt und arbeitet in Wien.
           |Kam in den 80er Jahren im Rahmen eines Auslandsstudiums
           |nach Österreich und blieb.
           |Absolvierte 1996 das Studium am Institut für Übersetzen
           |und Dolmetschen an der Universität Wien.
           |Seither freiberufliche Übersetzerin
           |mit den Schwerpunkten
           |Film, Kunst und Architektur.<br>
           |Mitglied der IG Übersetzerinnen Übersetzer, der Interessenvertretung der
           |literarischen und wissenschaftlichen ÜbersetzerInnen in Österreich.
           |</div>
           |<div class="bioAdr">
           |<div class="bioText">
           |<span class="bioTitle">Mag.</span><span class="bioName">KIMI LUM</span><br>
           |Ungargasse 65/9<br>
           |A-1030 Wien<br>
           |Österreich<br>
           |EUROPA<br>
           |</div>
           |</div>
           |<div class="bioText">
           |+43/(0)664/73 45 16 21<br>
           |office(at)kimilum.com<br>
           |</div>
           |<div class="bioCredit">Grafikdesign: Liz Pompe</div>
           |""".stripMargin
      case Eng =>
        s"""<div class="bioText">
           |Born in Hawaii, grew up in California,
           |lives and works in Vienna.
           |Came to Austria on a study abroad
           |program in the 80s and stayed.
           |Earned her master's degree
           |at the University of Vienna’s Institute for Translation
           |and Interpreting Studies in 1996.
           |Has worked since then as a freelance
           |translator mainly in the areas of
           |film, art, and architecture.<br>
           |Member of IG Übersetzerinnen Übersetzer, the Austrian Association
           |of Literary & Scientific Translators.
           |</div>
           |<div class="bioAdr">
           |<div class="bioText">
           |<span class="bioTitle">Mag.</span><span class="bioName">KIMI LUM</span><br>
           |Ungargasse 65/9<br>
           |A-1030 Vienna<br>
           |Austria<br>
           |EUROPE<br>
           |</div>
           |</div>
           |<div class="bioText">
           |+43/(0)664/73 45 16 21<br>
           |office(at)kimilum.com<br>
           |</div>
           |<div class="bioCredit">Graphic design: Liz Pompe</div>
           |""".stripMargin
    }
  }

  override def rendPagePrjCat(page: PrjCatPage, lang: Lang, device: Device): String = {
    def links(cat: Category): String = {
      val prjs = page.projects.filter { p => p.category.contains(cat) }
      val prjsSort = prjs.sortBy { x => x.title.value(lang).name.alph }
      prjsSort.map(p => {
        val page = ProjectPage(p)
        s"""<div class="prjCatLink"><a href="${fileName(page, lang)}">${pageNameContentHtml(page.name.value(lang))}</a></div>"""
      }).mkString("\n")
    }

    val usedCategories = page.projects.flatMap(p => p.category).distinct
    val ordered = usedCategories.sortBy(c => c.name.value(lang).toUpperCase())
    ordered.map(cat => {
      s"""<div class="prjCatHeading">${cat.name.value(lang).toUpperCase()}</div>
         |${links(cat)}
         |""".stripMargin
    }).mkString

  }

  override def rendPagePrjAlpha(page: PrjAlphaPage, lang: Lang, device: Device): String = {

    val prjSorted = page.projects.sortBy { x => x.title.value(lang).name.alph }

    val names = prjSorted.map { prj =>
      val p = ProjectPage(prj)
      s"""<div class="alphText"><a class="ainv" href="${fileName(p, lang)}">${pageNameHtml(p.name.value(lang))}</a></div>"""
    }.mkString("\n")
    s"""<div class="alphFill"></div>
       |$names
       |""".stripMargin
  }

  sealed trait TxtAlignment

  case object TA_LeftTop extends TxtAlignment

  case object TA_RightTop extends TxtAlignment

  case object TA_LeftBottom extends TxtAlignment

  case object TA_RightBottom extends TxtAlignment

  case class ImagePlacement(prj: Project, alignment: TxtAlignment, xoff: Int, yoff: Int, imgName: String)

  override def rendPageStart(page: StartPage, lang: Lang, device: Device): String = {

    def calcImageOffset(prj: Project, lang: Lang): ImagePlacement = {
      require(prj.title.value(lang).homepageAlph.isDefined)
      val img = prj.images.imageInfo(lang, ILStart)
      val w = img.width
      val cw = 60
      val ch = 75
      val top = 80
      val blockWidth = 300
      val blockHeight = 305
      val re = prj.title.value(lang).homepageAlph.get match {
        case 'A' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'B' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'C' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'D' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)

        case 'E' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)
        case 'F' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'G' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'H' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)
        case 'I' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)
        case 'J' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)

        case 'K' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'L' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'M' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'N' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)
        case 'O' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)

        case 'P' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'Q' => ImagePlacement(prj, TA_RightTop, -w + cw, -top, img.name)
        case 'R' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)
        case 'S' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)
        case 'T' => ImagePlacement(prj, TA_LeftTop, blockWidth - cw, -top, img.name)

        case 'U' => ImagePlacement(prj, TA_RightBottom, -w + cw, blockHeight - (3 * ch), img.name)
        case 'V' => ImagePlacement(prj, TA_RightBottom, (blockWidth / 2) - (w / 2), blockHeight - ch, img.name)
        case 'W' => ImagePlacement(prj, TA_LeftBottom, (blockWidth / 2) - (w / 2), blockHeight - ch, img.name)
        case 'X' => ImagePlacement(prj, TA_LeftBottom, (blockWidth / 2) - (w / 2), blockHeight - ch, img.name)
        case 'Z' => ImagePlacement(prj, TA_LeftBottom, blockWidth - cw, blockHeight - (3 * ch), img.name)
        case x => throw new IllegalStateException("Illegal hompageAlph character '%s'" format x)
      }
      re
    }

    def images(placements: List[ImagePlacement], lang: Lang): String = {
      val imgDivs = placements.map { p =>
        val startImageTextClass = p.alignment match {
          case TA_LeftTop => "startImageTextL"
          case TA_RightTop => "startImageTextR"
          case TA_LeftBottom => "startImageTextL"
          case TA_RightBottom => "startImageTextR"
        }
        val title = p.prj.startTitle.value(lang)
        val contrib = p.prj.contrib match {
          case None => ""
          case Some(c) => " | %s" format c.value(lang)
        }
        val credit = Photocredit.credit(lang, p.imgName) match {
          case None => ""
          case Some(c) => "%s" format c
        }

        def imgHeight = p.prj.images.imageInfo(lang, ILStart).height - 1

        def imgWidth = p.prj.images.imageInfo(lang, ILStart).width - 1

        def imageDiv: String =
          s"""<div class="startImageImage" style="background-image: url('images/${p.prj.images.imageInfo(lang, ILStart).name}'); width: ${imgWidth}px; height: ${imgHeight}px; " ></div>""".trim

        def imageTitleDiv: String =
          s"""<div class="$startImageTextClass">$title$contrib</div>""".trim

        def imageCreditDiv: String =
          s"""<div class="$startImageTextClass">$credit</div>""".trim

        def imageAllDiv: String = p.alignment match {
          case TA_LeftTop => imageTitleDiv + imageDiv + imageCreditDiv
          case TA_RightTop => imageTitleDiv + imageDiv + imageCreditDiv
          case TA_LeftBottom => imageCreditDiv + imageDiv + imageTitleDiv
          case TA_RightBottom => imageCreditDiv + imageDiv + imageTitleDiv
        }

        s"""<div class="startImage" style="width: ${p.prj.images.imageInfo(lang, ILStart).width}px; " id="img_${p.prj.id}">
           |$imageAllDiv
           |</div>
           |""".stripMargin
      }.mkString("\n")

      s"""<div>
         |$imgDivs
         |</div>
         |""".stripMargin
    }

    def script(placements: List[ImagePlacement], lang: Lang): String = {
      def defElem: String = placements.map(p => {
        s"""var elem = $$( "#startAphaBlock" ); var height = elem.height(); var offset = elem.offset();
           |$$( "#img_${p.prj.id}" ).css({top: offset.top + ${p.yoff}, left: offset.left + ${p.xoff}, position:'absolute'});
           |""".stripMargin.trim()
      }).mkString("\n")

      def fade: String = page.projects.map(p => {
        s"""$$( "#char_${p.id}" ).mouseenter(function() { $$( "#img_${p.id}" ).fadeIn( 500 );});
           |$$( "#char_${p.id}" ).mouseleave(function() { $$( "#img_${p.id}" ).fadeOut( 500 );});
           |""".stripMargin.trim()
      }).mkString("\n")

      s"""<script>
         |$$( document ).ready(function() {
         |  {
         |$defElem
         |  }
         |});
         |$fade
         |</script>
         |""".stripMargin
    }

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


    val placements = page.projects.filter(_.title.value(Ger).homepageAlph.isDefined).map(calcImageOffset(_, Ger))
    s"""<div id="startAphaBlock">
       |<div>${alph('A', 'E', lang)}</div>
       |<div>${alph('F', 'J', lang)}</div>
       |<div>${alph('K', 'O', lang)}</div>
       |<div>${alph('P', 'T', lang)}</div>
       |<div>${alph1(List('U', 'V', 'W', 'X', 'Z'), lang)}</div>
       |</div>
       |<div id="startAlphaBlockBack"></div>
       |${images(placements, lang)}
       |${script(placements, lang)}
       |""".stripMargin
  }

  override def rendPageProject(page: ProjectPage, lang: Lang, device: Device): String = {

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
      s"""${prj.company.get.typ.name.value(lang)}: ${prj.company.get.name}"""
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

    def paddingLeft(lang: Lang): Int = prj.images.imageInfo(lang, ILProject).width + 35

    def subtitleHtml(lang: Lang): String = {

      prj.subTitle match {
        case None => ""
        case Some(st) => s"""<div style="padding-left:${paddingLeft(lang)}px" class="prjSubtitle">${st.value(lang)}</div>"""
      }

    }

    def contentDefault(lang: Lang): String =
      s"""<img class="prjImg" src="images/${prj.images.imageInfo(lang, ILProject).name}"/>
         |<div class="prjBody">
         |<div class="prjTitle">${prj.title.value(lang).name.getUqual.toUpperCase()} ${beitrag(lang)}</div>
         |${subtitleHtml(lang)}
         |<div style="padding-left:${paddingLeft(lang)}px" class="prjText">
         |${details(lang)}
         |</div>
         |</div>
         |""".stripMargin

    if (SpecialProjectPage.isSpecialPage(prj.id)) SpecialProjectPage.specialPage(page.id, lang)
    else contentDefault(lang)
  }

  override def rendBody(currentPage: Page, pages: List[Page], lang: Lang, languages: List[Lang], pageContent: DeviceData[String]): DeviceData[String] = {

    def menuItemClass(loc: Location, active: Boolean): String =
      if (active) "menuItemF" // Do not draw the short vertical line for the active menu item
      else loc match {
        case LOC_First => "menuItemF"
        case LOC_Middle => "menuItemM"
        case LOC_Last => "menuItemL"
      }

    def menuLangItemClass(loc: Location): String = loc match {
      case LOC_First => "menuLangItemF"
      case LOC_Middle => "menuLangItemM"
      case LOC_Last => "menuLangItemL"
    }

    def menuInfo(current: Page, relevant: List[MenuPage], lang: Lang): String = {
      val li = relevant.map(menuPage => {
        val active = current.id == menuPage.page.id
        val clazz = menuItemClass(menuPage.location, active)
        val idAttr = menuPage.id match {
          case None => ""
          case Some(id) => s"""id="$id" """
        }
        val style = if (current.pageType != PT_Project || menuPage.id.isEmpty || menuPage.id.get != "prjAlphaPage") ""
        else
          """ style="padding-bottom: 600px" """

        if (menuPage.page != current)
          s"""<div $idAttr $style class="$clazz"><a href="${fileName(menuPage.page, lang)}">${pageNameHtml(menuPage.page.name.value(lang))}</a></div>"""
        else
          s"""<div $idAttr $style class="$clazz">${pageNameHtml(menuPage.page.name.value(lang))}</div>"""
      })
      val m = li.mkString("")
      s"""<div id="infoMenu"><div class="menuItemFill"></div>$m</div>"""
    }

    def menuLang(current: Page, currentLang: Lang, languages: List[MenuLang]): String = {
      val oredered = languages.sortBy(_.lang.menuSortOrder)
      val li = oredered.map(l => {
        val clazz = menuLangItemClass(l.location)
        if (l.lang != currentLang)
          s"""<span class="$clazz"><a href="${fileName(current, l.lang)}">${l.lang.name}</a></span>"""
        else
          s"""<span class="$clazz">${l.lang.name}</span>"""
      })
      val m = li.mkString("")
      s"""<div class="langMenu">$m</div>"""
    }

    val contentId = currentPage.pageType match {
      case PT_Start => "contentStart"
      case PT_Project => "contentPrj"
      case _ => "content"
    }

    def gerTxt(pContent: String) =
      s"""<div class="headline">
         |<span class="headline1">KIMI LUM</span>
         |<span class="headline2">ÜBERSETZERIN</span>
         |<span class="headline3">aus dem Deutschen ins Englische</span>
         |${menuLang(currentPage, lang, menuLangList(languages))}
         |</div>
         |${menuInfo(currentPage, menuPages(pages), lang)}
         |<div id="$contentId">
         |$pContent
         |</div>
         |""".stripMargin

    def engTxt(pContent: String) =
      s"""<div class="headline">
         |<span class="headline1">KIMI LUM</span>
         |<span class="headline2">TRANSLATOR</span>
         |<span class="headline3">from German to English</span>
         |${menuLang(currentPage, lang, menuLangList(languages))}
         |</div>
         |${menuInfo(currentPage, menuPages(pages), lang)}
         |<div id="$contentId">
         |$pContent
         |</div>
         |""".stripMargin

    MultiLangGeneric[DeviceData[String]](
      pageContent.copy(data = gerTxt(pageContent.data)),
      pageContent.copy(data = engTxt(pageContent.data))
    ).value(lang)
  }


}
