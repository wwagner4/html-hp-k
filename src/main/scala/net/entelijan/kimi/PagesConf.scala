package net.entelijan.kimi

import net.entelijan.kimi.Homepage.{categories, languages, projects}
import net.entelijan.kimi.Model._

object PagesConf {

  class StartPage(pages: => List[Page], projects: List[Project], languages: List[Lang]) extends Page {

    sealed trait TxtAlignment

    case object TA_LeftTop extends TxtAlignment

    case object TA_RightTop extends TxtAlignment

    case object TA_LeftBottom extends TxtAlignment

    case object TA_RightBottom extends TxtAlignment

    case class ImagePlacement(prj: Project, alignment: TxtAlignment, xoff: Int, yoff: Int, imgName: String)

    import HtmlTemplateEngine._

    private val self = this

    def id = "index"

    def name = MultiLangGeneric(PageName(None, "START"), PageName(None, "HOME"))

    def pageType = Start

    def menuSortOrder = 0

    def alph(from: Char, to: Char, lang: Lang) = (from to to).map(c => createChar(c, lang)).mkString("\n")

    def alph1(chars: List[Char], lang: Lang) = chars.map(c => createChar(c, lang)).mkString("\n")

    def createChar(c: Char, lang: Lang): String = {
      val prjs = projects.filter(p => {
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

    def htmlText = new MultiLang[String] {

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
            s"""
<div class="startImageImage" style="background-image: url('images/${p.prj.images.imageInfo(lang, ILStart).name}'); width: ${imgWidth}px; height: ${imgHeight}px; " ></div>
""".trim

          def imageTitleDiv: String =
            s"""
<div class="$startImageTextClass">${title}${contrib}</div>
""".trim

          def imageCreditDiv: String =
            s"""
<div class="$startImageTextClass">${credit}</div>
""".trim

          def imageAllDiv: String = p.alignment match {
            case TA_LeftTop => imageTitleDiv + imageDiv + imageCreditDiv
            case TA_RightTop => imageTitleDiv + imageDiv + imageCreditDiv
            case TA_LeftBottom => imageCreditDiv + imageDiv + imageTitleDiv
            case TA_RightBottom => imageCreditDiv + imageDiv + imageTitleDiv
          }

          s"""
<div class="startImage" style="width: ${p.prj.images.imageInfo(lang, ILStart).width}px; " id="img_${p.prj.id}">
$imageAllDiv
</div>
      """
        }.mkString("\n")

        s"""
<div>
${imgDivs}
</div>
        """
      }

      def script(placements: List[ImagePlacement], lang: Lang): String = {
        def defElem: String = placements.map(p => {
          s"""
var elem = $$( "#startAphaBlock" ); var height = elem.height(); var offset = elem.offset();
$$( "#img_${p.prj.id}" ).css({top: offset.top + ${p.yoff}, left: offset.left + ${p.xoff}, position:'absolute'});
          """.trim()
        }).mkString("\n")

        def fade: String = prjs.map(p => {
          s"""
$$( "#char_${p.id}" ).mouseenter(function() { $$( "#img_${p.id}" ).fadeIn( 500 );});
$$( "#char_${p.id}" ).mouseleave(function() { $$( "#img_${p.id}" ).fadeOut( 500 );});
          """.trim()
        }).mkString("\n")

        s"""
<script>
$$( document ).ready(function() {
  {
${defElem}
  }
});
${fade}
</script>
            """;
      }

      def startPageTemplate(page: Page, lang: Lang): String = {
        val placements = prjs.filter(_.title.value(lang).homepageAlph.isDefined).map(calcImageOffset(_, lang))
        pageTemplate(self, pages, lang, languages,
          s"""
<div id="startAphaBlock">
<div>${alph('A', 'E', lang)}</div>
<div>${alph('F', 'J', lang)}</div>
<div>${alph('K', 'O', lang)}</div>
<div>${alph('P', 'T', lang)}</div>
<div>${alph1(List('U', 'V', 'W', 'X', 'Z'), lang)}</div>
</div>
<div id="startAlphaBlockBack"></div>
${images(placements, lang)}
${script(placements, lang)}
    """)
      }

      def ger = startPageTemplate(self, Ger)

      def eng = startPageTemplate(self, Eng)
    }

  }

  class BioContactPage(pages: => List[Page], languages: List[Lang]) extends Page {

    import HtmlTemplateEngine._

    val self = this

    def id = "bio"

    def name = MultiLangGeneric(PageName(None, "LEBENSLAUF / KONTAKT"), PageName(None, "BIO / CONTACT"))

    def menuSortOrder = 60

    override def menuId = Some(id)

    def pageType = Info

    def htmlText = new MultiLang[String] {

      def ger = pageTemplate(self, pages, Ger, languages,
        s"""
<div class="bioText">
Geboren in Hawaii, USA, aufgewachsen in Kalifornien,<br>
lebt und arbeitet in Wien.<br>
Kam in den 80er Jahren im Rahmen eines Auslandsstudiums<br>
nach Österreich und blieb.<br>
Absolvierte 1996 das Studium am Institut für Übersetzen<br>
und Dolmetschen an der Universität Wien.<br>
Seither freiberufliche Übersetzerin<br>
mit den Schwerpunkten<br>
Film, Kunst und Architektur.
</div>

<div class="bioAdr">
<div class="bioText">
<span class="bioTitle">Mag.</span><span class="bioName">KIMI LUM</span><br>
Ungargasse 65/9<br>
A-1030 Wien<br>
Österreich<br>
EUROPA<br>
</div>
</div>

<div class="bioText">
+43/(0)664/73 45 16 21<br>
office(at)kimilum.com<br>
</div>
<div class="bioCredit">Grafikdesign: Liz Pompe</div>

          """)

      def eng = pageTemplate(self, pages, Eng, languages,
        s"""
<div class="bioText">
Born in Hawaii, grew up in California,<br>
lives and works in Vienna.<br>
Came to Austria on a study abroad<br>
program in the 80s and stayed.<br>
Earned her master's degree<br>
at the University of Vienna’s Institute for Translation<br>
and Interpreting Studies in 1996.<br>
Has worked since then as a freelance<br>
translator mainly in the areas of<br>
film, art, and architecture.
</div>

<div class="bioAdr">
<div class="bioText">
<span class="bioTitle">Mag.</span><span class="bioName">KIMI LUM</span><br>
Ungargasse 65/9<br>
A-1030 Vienna<br>
Austria<br>
EUROPE<br>
</div>
</div>

<div class="bioText">
+43/(0)664/73 45 16 21<br>
office(at)kimilum.com<br>
</div>
<div class="bioCredit">Graphic design: Liz Pompe</div>
          """)
    }
  }

  class PrjAlphaPage(pages: => List[Page], projects: List[Project], languages: List[Lang]) extends Page {

    import HtmlTemplateEngine._

    val self = this

    def id = "prjAlphaPage"

    def name = MultiLangGeneric(PageName(Some("Projekte nach"), "TITEL"), PageName(Some("Projects by"), "TITLE"))

    def menuSortOrder = 10

    override def menuId = Some(id)

    def pageType = ProjectOverview

    def content(lang: Lang): String = {
      def projectsFrom(c: Char): String = {

        val prjs = projects.filter(p => p.title.value(lang).name.alph(0).toUpper == c.toUpper)
        prjs.map(prj => {
          val p = projectToPage(prj, pages, languages)
          s"""<a href="${fileName(p, lang)}">${pageNameHtml(p.name.value(lang))}</a>"""
        }).mkString(" ")
      }

      val prjSorted = projects.sortBy { x => x.title.value(lang).name.alph }

      val names = prjSorted.map { prj =>
        val p = projectToPage(prj, pages, languages)
        s"""<div class="alphText"><a class="ainv" href="${fileName(p, lang)}">${pageNameHtml(p.name.value(lang))}</a></div>"""
      }.mkString("\n")
      s"""
          <div class="alphFill"></div>
          $names
        """

    }

    def htmlText = new MultiLang[String] {
      def ger = pageTemplate(self, pages, Ger, languages, content(Ger))

      def eng = pageTemplate(self, pages, Eng, languages, content(Eng))
    }
  }

  class PrjCatPage(pages: => List[Page], projects: List[Project], categories: List[Category], languages: List[Lang]) extends Page {

    import HtmlTemplateEngine._

    val self = this

    def id = "prjCatPage"

    def name = MultiLangGeneric(PageName(Some("Projekte nach"), "KATEGORIE"), PageName(Some("Projects by"), "CATEGORY"))

    def menuSortOrder = 20

    override def menuId = Some(id)

    def pageType = ProjectOverview

    def content(lang: Lang): String = {
      def links(cat: Category): String = {
        val prjs = projects.filter { p => p.category.contains(cat) }
        val prjsSort = prjs.sortBy { x => x.title.value(lang).name.alph }
        prjsSort.map(p => {
          val page = projectToPage(p, pages, languages)
          s"""<div class="prjCatLink"><a href="${fileName(page, lang)}">${pageNameContentHtml(page.name.value(lang))}</a></div>"""
        }).mkString("\n")
      }

      val usedCategories = projects.flatMap(p => p.category).toSet.toList
      val ordered = usedCategories.sortBy(c => c.name.value(lang).toUpperCase())
      ordered.map(cat => {
        s"""
<div class="prjCatHeading">${cat.name.value(lang).toUpperCase()}</div>
${links(cat)}
          """
      }).mkString

    }

    def htmlText = new MultiLang[String] {
      def ger = pageTemplate(self, pages, Ger, languages, content(Ger))

      def eng = pageTemplate(self, pages, Eng, languages, content(Eng))
    }
  }

  class PrjAuthPage(pages: => List[Page], projects: List[Project], languages: List[Lang]) extends Page {

    import HtmlTemplateEngine._

    val self = this

    def id = "prjAuthPage"

    def name = MultiLangGeneric(PageName(Some("Projekte nach"), "AUTOR"), PageName(Some("Projects by"), "AUTHOR"))

    def menuSortOrder = 15

    override def menuId = Some(id)

    def pageType = ProjectOverview

    def content(lang: Lang): String = {

      def links(artist: Artist): String = {
        val prjs = projects.filter { p =>
          (p.artist.foldRight(false)((a, b) => b || a.name == artist.name)) && // What is that condition good for ??
            artist.role != AR_Undef$ // Ignore artists with undefined role
        }
        val prjsSort = prjs.sortBy { x => x.title.value(lang).name.alph }
        prjsSort.map(p => {
          val page = projectToPage(p, pages, languages)
          s"""<div class="prjCatLink"><a href="${fileName(page, lang)}">${pageNameContentHtml(page.name.value(lang))}</a></div>"""
        }).mkString("")
      }

      val usedAuthors = projects.flatMap(p => p.artist).toSet.toList
      val ordered = usedAuthors.sortBy {
        _.name.toUpperCase()
      }

      ordered.map(artist => {
        s"""
<div class="prjCatHeading">${artist.name.toUpperCase()}</div>
${links(artist)}"""
      }).mkString
    }

    def htmlText = new MultiLang[String] {
      def ger = pageTemplate(self, pages, Ger, languages, content(Ger))

      def eng = pageTemplate(self, pages, Eng, languages, content(Eng))
    }
  }

  val prjs = projects.result
  private val startPage: Page = new StartPage(pages, prjs, languages)
  private val bioContactPage: Page = new BioContactPage(pages, languages)
  private val prjAlphaPage: Page = new PrjAlphaPage(pages, prjs, languages)
  private val prjCatPage: Page = new PrjCatPage(pages, prjs, categories, languages)
  private val prjAuthPage: Page = new PrjAuthPage(pages, prjs, languages)
  val pages: List[Page] = List(startPage, bioContactPage, prjAlphaPage, prjCatPage, prjAuthPage)

}
