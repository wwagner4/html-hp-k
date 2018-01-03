package net.entelijan.kimi

import java.io._
import HtmlTemplateEngine._
import Implementation._

object Implementation {

  trait ReportBody

  trait ReportLines extends ReportBody {
    def lines: List[String]
  }

  case class ReportLinesImpl(lines: List[String]) extends ReportLines

  trait Report extends ReportBody {
    def heading: Option[String]
    def body: List[ReportBody]
  }

  trait CreationResult[T] {
    def report: Report
    def result: T
  }

  trait ArtistList {
    def render(lang: Lang): String
    def artists: List[Artist1]
  }

  sealed trait ArtistRole1 {
    def masculineSingular: MultiLang[String]
    def femininSingular: MultiLang[String]
    def masculinePlural: MultiLang[String]
    def femininPlural: MultiLang[String]
    def mixedPlural: MultiLang[String]
    def order: Int  
  }
  case object AR1_Director extends ArtistRole1 {
    def masculineSingular = MultiLangString(ger = "Regie:", eng = "Director:")
    def femininSingular = MultiLangString(ger = "Regie:", eng = "Director:")
    def masculinePlural = MultiLangString(ger = "Regie:", eng = "Directors:")
    def femininPlural = MultiLangString(ger = "Regie:", eng = "Directors:")
    def mixedPlural = MultiLangString(ger = "Regie:", eng = "Directors:")
    def order = 50
  }
  case object AR1_Editor extends ArtistRole1 {
    def masculineSingular = MultiLangString(ger = "Herausgeber:", eng = "Editor:")
    def femininSingular = MultiLangString(ger = "Herausgeberin:", eng = "Editor:")
    def masculinePlural = MultiLangString(ger = "Herausgeber:", eng = "Editors:")
    def femininPlural = MultiLangString(ger = "Herausgeberinnen:", eng = "Editors:")
    def mixedPlural = MultiLangString(ger = "Herausgegeben von", eng = "Editors:")
    def order = 40
  }
  case object AR1_Author extends ArtistRole1 {
    def masculineSingular = MultiLangString(ger = "Autor:", eng = "Author:")
    def femininSingular = MultiLangString(ger = "Autorin:", eng = "Author:")
    def masculinePlural = MultiLangString(ger = "Autoren:", eng = "Authors:")
    def femininPlural = MultiLangString(ger = "Autorinnen:", eng = "Authors:")
    def mixedPlural = MultiLangString(ger = "Autoren:", eng = "Authors:")
    def order = 20
  }
  case object AR1_Artist extends ArtistRole1 {
    def masculineSingular = MultiLangString(ger = "Künstler:", eng = "Artist:")
    def femininSingular = MultiLangString(ger = "Künstlerin:", eng = "Artist:")
    def masculinePlural = MultiLangString(ger = "Künstler:", eng = "Artists:")
    def femininPlural = MultiLangString(ger = "Künstlerinnen:", eng = "Artists:")
    def mixedPlural = MultiLangString(ger = "Künstler:", eng = "Artists:")
    def order = 30
  }
  case object AR1_Architect extends ArtistRole1 {
    def masculineSingular = MultiLangString(ger = "Architekt:", eng = "Architect:")
    def femininSingular = MultiLangString(ger = "Architektin:", eng = "Architect:")
    def masculinePlural = MultiLangString(ger = "Architekten:", eng = "Architects:")
    def femininPlural = MultiLangString(ger = "Architektinnen:", eng = "Architects:")
    def mixedPlural = MultiLangString(ger = "Architekten:", eng = "Architects:")
    def order = 10
  }
  case object AR1_Undef extends ArtistRole1 {
    def masculineSingular = MultiLangString(ger = "", eng = "")
    def femininSingular = MultiLangString(ger = "", eng = "")
    def masculinePlural = MultiLangString(ger = "", eng = "")
    def femininPlural = MultiLangString(ger = "", eng = "")
    def mixedPlural = MultiLangString(ger = "", eng = "")
    def order = 11
  }

  case class Artist1(gender: Gender, role: ArtistRole1, name: String, nameReverse: String) {
    override def equals(o: Any) = o match {
      case that: Artist1 => that.name.equalsIgnoreCase(this.name)
      case _             => false
    }
    override def hashCode = 1
  }

  sealed trait Gender
  case object G_Masculin extends Gender
  case object G_Feminin extends Gender
  case object G_Undef extends Gender

  sealed trait CompanyType {
    def name: MultiLang[String]
  }
  case object Comp_R extends CompanyType {
    def name = MultiLangString("Verlag", "Publisher")
  }
  case object Comp_P extends CompanyType {
    def name = MultiLangString("Produktion", "Production company")
  }
  case object Comp_M extends CompanyType {
    def name = MultiLangString("Museum", "Museum")
  }
  case object Comp_E extends CompanyType {
    def name = MultiLangString("Editor", "Herausgeber")
  }
  case object Comp_L extends CompanyType {
    def name = MultiLangString("Label", "Label")
  }
  case object Comp_UNDEF extends CompanyType {
    def name = MultiLangString("", "")
  }
  trait Company {
    def name: String
    def typ: CompanyType
  }

  sealed trait Lang {
    def name: String
    def id: String
    def menuSortOrder: Int
  }
  case object Eng extends Lang {
    def name = "EN"
    def id = "eng"
    def menuSortOrder = 30
  }
  case object Ger extends Lang {
    def name = "DE"
    def id = "ger"
    def menuSortOrder = 20
  }

  sealed trait ImageLocation
  case object ILStart extends ImageLocation
  case object ILProject extends ImageLocation

  trait ImageLangLoc {
    def imageInfo(lang: Lang, loc: ImageLocation): ImageInfo
  }

  trait ImageInfo {
    def name: String
    def width: Int
    def height: Int
  }

  trait AlphString {
    def alph: String
    def get: String
    def getUqual: String
  }

  case class AlphStringImpl(alph: String, get: String, getUqual: String) extends AlphString

  case class AlphStringSimple(get: String, getUqual: String) extends AlphString {
    def alph = get match {
      case "" => "ZZZZZZ"
      case _  => get.trim().toUpperCase()
    }
  }

  trait MultiLang[T] {

    def ger: T

    def eng: T

    def value(lang: Lang): T = {
      lang match {
        case Eng => eng
        case Ger => ger
      }
    }
  }

  trait MultiLangSimple[T] extends MultiLang[T] {

    def ger: T = value(Ger)

    def eng: T = value(Eng)

    def value(lang: Lang): T
  }

  case class MultiLangString(ger: String, eng: String) extends MultiLang[String]

  case class MultiLangGeneric[T](ger: T, eng: T) extends MultiLang[T]

  case class MultiLangStringSimple(value: String) extends MultiLang[String] {
    def ger = value
    def eng = value
  }

  case class MultiLangAlphString(ger: AlphString, eng: AlphString) extends MultiLang[AlphString]

  case class MultiLangChar(ger: Char, eng: Char) extends MultiLang[Char]

  sealed trait Category {
    def name: MultiLang[String]
  }

  case object Cat_A extends Category {
    def name = MultiLangString("Architektur", "Architecture")
  }
  case object Cat_M extends Category {
    def name = MultiLangString("Film", "Film")
  }
  case object Cat_D extends Category {
    def name = MultiLangString("Kunst | Design", "Art | Design")
  }
  case object Cat_L extends Category {
    def name = MultiLangString("Literatur", "Literature")
  }
  case object Cat_S extends Category {
    def name = MultiLangString("Sachbuch", "Nonfiction")
  }
  case object Cat_U extends Category {
    def name = MultiLangString("Musik", "Music")
  }

  object ArtistRolesUtil {
    def parse(roles: String, names: String, namesRev: String): List[Artist1] = {
      def roleNameToArtist(rn: (String, (String, String))): Artist1 = {

        def parseGender(code: String): Gender =
          if (code.startsWith("fem")) G_Feminin
          else if (code.startsWith("mas")) G_Masculin
          else G_Undef

        def parseRole(code: String): ArtistRole1 =
          if (code.endsWith("D")) AR1_Director
          else if (code.endsWith("E")) AR1_Editor
          else if (code.endsWith("W")) AR1_Author
          else if (code.endsWith("A")) AR1_Artist
          else if (code.endsWith("B")) AR1_Architect
          else AR1_Undef

        val code = rn._1.trim()
        def gender = parseGender(code)
        def role = parseRole(code)
        def name = rn._2._1.trim()
        def nameRev = rn._2._2.trim()

        Artist1(gender, role, name, nameRev)
      }

      val rolesSplit = roles.split("/").toList
      val namesSplit = names.split("/").toList
      val namesRevSplit = namesRev.split("/").toList

      if (rolesSplit.size < 1) throw new IllegalStateException(s"'$roles' contain no value(s)")
      if (namesSplit.size < 1) throw new IllegalStateException(s"'$names' contain no value(s)")
      if (namesRevSplit.size < 1) throw new IllegalStateException(s"'$namesRev' contain no value(s)")

      if (namesSplit.size != namesRevSplit.size) throw new IllegalStateException(s"'$names' and '$namesRev' do not contain the same number of values")
      if (namesSplit.size != rolesSplit.size) throw new IllegalStateException(s"'$names' and '$roles' do not contain the same number of values")

      val nn = namesSplit.zip(namesRevSplit)
      val rns = rolesSplit.zip(nn)
      rns.map(rn => roleNameToArtist(rn))
    }

    def format(artists: List[Artist1], lang: Lang): List[String] = {
      def roleGroupsToString(rg: List[Artist1], lang: Lang): String = {
        val g = rg(0)

        def allFeminin: Boolean = rg.foldLeft(true)((actual, artist) => actual && artist.gender == G_Feminin)
        def allMasculin: Boolean = rg.foldLeft(true)((actual, artist) => actual && artist.gender == G_Masculin)

        def heading: String =
          if (rg.size == 1) {
            rg(0).gender match {
              case G_Feminin  => g.role.femininSingular.value(lang)
              case G_Masculin => g.role.masculineSingular.value(lang)
              case G_Undef    => ""
            }
          } else if (allFeminin) g.role.femininPlural.value(lang)
          else if (allMasculin) g.role.masculinePlural.value(lang)
          else g.role.mixedPlural.value(lang)
        def names = rg.map { _.nameReverse }.mkString(", ")

        "%s %s" format (heading, names)

      }
      if (artists.isEmpty) throw new IllegalStateException("Artists list must never be empty")
      val roles = artists.groupBy { x => x.role }
      val orderedRoles = {
        val orderedKeys = roles.keys.toList.sortBy { _.order }
        orderedKeys.map { roles(_) }
      }
      roles.values.toList.map(rg => roleGroupsToString(rg, lang))
    }

  }

  sealed trait PageType
  case object Start extends PageType
  case object ProjectOverview extends PageType
  case object ProjectPage extends PageType
  case object Info extends PageType

  case class PageName(prefix: Option[String], name: String)

  trait Page {
    def id: String
    def name: MultiLang[PageName]
    def pageType: PageType
    def menuSortOrder: Int
    def htmlText: MultiLang[String]

    def menuId = pageType match {
      case ProjectOverview => Some(id)
      case _               => None
    }
  }

  trait ProjectTitle {
    def homepageAlph: Option[Char] = None
    def name: AlphString
  }

  trait Project {
    def id: String
    def title: MultiLang[ProjectTitle]
    def startTitle: MultiLang[String]
    def subTitle: Option[MultiLang[String]]
    def contrib: Option[MultiLang[String]]
    def artist: List[Artist1]
    def company: Option[Company]
    def isbn: Option[String]
    def category: List[Category]
    def images: ImageLangLoc
    def projectDetails: Option[MultiLang[String]] = None
    def year: Option[String] = None

  }

  object ImageLangLocFactory {

    def toImageLangLoc(files: List[ImageInfo]): ImageLangLoc = {

      case class NameAnalyzed(lang: Option[Lang], loc: Option[ImageLocation], name: ImageInfo)

      def bestFit(lang: Lang, loc: ImageLocation, names: List[NameAnalyzed]): ImageInfo = {

        def fitVal(lang: Lang, loc: ImageLocation, name: NameAnalyzed): Int = {
          val re = name match {
            case NameAnalyzed(Some(langNam), Some(locNam), _) =>
              if (langNam == lang && locNam == loc) 100
              else if (langNam == lang) 60
              else if (locNam == loc) 60
              else 0
            case NameAnalyzed(None, Some(locNam), _) =>
              if (locNam == loc) 80
              else 10
            case NameAnalyzed(Some(langNam), None, _) =>
              if (langNam == lang) 80
              else 10
            case _ => 50
          }
          //println("%5s %10s - %16s -> %d" format (lang, loc, name.name, re))
          re
        }
        def bestFit1(lang: Lang, loc: ImageLocation, nam1: NameAnalyzed, nam2: NameAnalyzed): NameAnalyzed = {
          val f1 = fitVal(lang, loc, nam1)
          val f2 = fitVal(lang, loc, nam2)
          if (f1 > f2) nam1 else nam2
        }
        names.reduce(bestFit1(lang, loc, _, _)).name
      }

      def analyze(names: List[ImageInfo]): List[NameAnalyzed] = {

        val EngStartRegex = ".*EN_START.*".r
        val GerStartRegex = ".*DE_START.*".r
        val EngPrjRegex = ".*EN_PRJ.*".r
        val GerPrjRegex = ".*DE_PRJ.*".r
        val EngRegex = ".*EN.*".r
        val GerRegex = ".*DE.*".r
        val StartRegex = ".*START.*".r
        val PrjRegex = ".*PRJ.*".r
        val AnyRegex = ".*".r

        names.map { imgInfo =>
          imgInfo.name match {
            case EngStartRegex() => NameAnalyzed(Some(Eng), Some(ILStart), imgInfo)
            case GerStartRegex() => NameAnalyzed(Some(Ger), Some(ILStart), imgInfo)
            case EngPrjRegex()   => NameAnalyzed(Some(Eng), Some(ILProject), imgInfo)
            case GerPrjRegex()   => NameAnalyzed(Some(Ger), Some(ILProject), imgInfo)
            case GerRegex()      => NameAnalyzed(Some(Ger), None, imgInfo)
            case EngRegex()      => NameAnalyzed(Some(Eng), None, imgInfo)
            case StartRegex()    => NameAnalyzed(None, Some(ILStart), imgInfo)
            case PrjRegex()      => NameAnalyzed(None, Some(ILProject), imgInfo)
            case AnyRegex()      => NameAnalyzed(None, None, imgInfo)
          }
        }
      }
      require(files.size > 0)

      val analyzed = analyze(files)

      new ImageLangLoc {

        lazy val gerStart = bestFit(Ger, ILStart, analyzed)
        lazy val engStart = bestFit(Eng, ILStart, analyzed)
        lazy val gerPrj = bestFit(Ger, ILProject, analyzed)
        lazy val engPrj = bestFit(Eng, ILProject, analyzed)

        def imageInfo(lang: Lang, loc: ImageLocation): ImageInfo = {
          lang match {
            case Ger => loc match {
              case ILStart   => gerStart
              case ILProject => gerPrj
            }
            case Eng => loc match {
              case ILStart   => engStart
              case ILProject => engPrj
            }
          }
        }
      }
    }
  }

}

object HtmlTemplateEngine {

  import Implementation._

  def pageNameHtml(name: PageName): String = name.prefix match {
    case None         => s"""<span class="infoMenuItemText">${name.name.toUpperCase()}</span>"""
    case Some(prefix) => s"""<span class="infoMenuItemTextPrefix">${prefix}</span><span class="infoMenuItemText">${name.name.toUpperCase()}</span>"""
  }

  def pageNameContentHtml(name: PageName): String = name.prefix match {
    case None         => s"""${name.name}"""
    case Some(prefix) => s"""${prefix} ${name.name}"""
  }

  def bodyTemplate(bodyContent: String, page: Page, lang: Lang): String = {
    val bodyStyle =
      if (page.pageType == Start) """style="overflow:hidden;" """
      else ""

    val reloadScript =
      if (page.pageType == Start) """
<script type="text/javascript">
function doPageReload() {
    if(isMobile()) {
      // Nothing to do  
    } else {
      window.location.href = window.location.href;
    }
};

var resizeTimer;
$(window).resize(function() {
    if (!isMobile()) {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(doPageReload, 100);
    }
});
</script>
"""
      else ""

    val alignContentScript =
      if (page.menuId.isDefined) s"""
<script type="text/javascript">
function doAlignContent() {
    // console.log("doAlignContent");
    var mi = $$("#${page.menuId.get}");
    var x = mi.offset().left;
    var y = mi.offset().top;
    var h = mi.height();
    //console.log("#${page.menuId.get} " + x + " " + y + " " + h);
    var c = $$("#content");
    c.css("left", "" + x + "px");
    c.css("top", "" + y + "px");
    var m = $$("#infoMenu")
    if (m.width() >= 1200) c.width(m.width() - 40)
    else c.width(m.width() - x - 40)
};

$$(window).load(function() {
  doAlignContent();
});
$$(document).ready(function() {
  doAlignContent();
});
$$(window).resize(function() {
  if (!isMobile()) {
    doAlignContent();
  }
});
</script>
"""
      else ""

    val alignPrjPageScript =
      if (page.pageType == ProjectPage) s"""
<script type="text/javascript">
function doAlignPrjPage() {
    var pa = $$("#page");
    //console.log("pa " + pa);
    if (pa.offset()) {
      // console.log("pa offset " + pa.offset());
      var y = pa.offset().left + 60;
      // console.log("prj offset " + y);
      var c = $$("#contentPrj");
      c.css("left", "" + y + "px");
    } else {
      // console.log("pa offset NOT READY");
    }
};

$$(window).load(function() {
  doAlignPrjPage();
});
$$(document).ready(function() {
  doAlignPrjPage();
});
$$(window).resize(function() {
  if (!isMobile()) {
    doAlignPrjPage();
  }
});
</script>
"""
      else ""

    s"""
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Kimi Lum ${page.name.value(lang).name}</title>
<style type="text/css">
${Css.all}
</style>
<script type="text/javascript" src="js/jquery-1.11.1.min.js"></script>
<script type="text/javascript">
function isMobile() {
    return /Edge|Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
}
</script>
$reloadScript
$alignContentScript
$alignPrjPageScript
</head>
<body $bodyStyle >
<div id="page">
$bodyContent
</div>
</body>
</html>
"""
  }

  sealed trait Location
  case object LOC_First extends Location
  case object LOC_Middle extends Location
  case object LOC_Last extends Location

  case class MenuPage(page: Page, location: Location, id: Option[String])
  case class MenuLang(lang: Lang, location: Location)

  def pageTemplate(currentPage: Page, pages: List[Page], lang: Lang, languages: List[Lang], pageContent: String): String = {

    def locations(size: Int): List[Location] = {
      (1 to size).toList map { i =>
        if (i == 1) LOC_First
        else if (i >= size) LOC_Last
        else LOC_Middle
      }
    }

    def convertToMenuPages(pages: List[Page]): List[MenuPage] = {
      val locs = locations(pages.size)
      pages.zip(locs) map { case (page, loc) => MenuPage(page, loc, page.menuId) }
    }

    def convertToMenuLangList(languages: List[Lang]): List[MenuLang] = {
      val locs = locations(languages.size)
      languages.zip(locs) map { case (a, b) => MenuLang(a, b) }
    }

    val rel = pages.filter { page => page.pageType == Start || page.pageType == ProjectOverview || page.pageType == Info }
    val relOrdered = rel.sortBy(_.menuSortOrder)
    val menuPages = convertToMenuPages(relOrdered)
    val menuLangList = convertToMenuLangList(languages)
    val contentId = currentPage.pageType match {
      case Start       => "contentStart"
      case ProjectPage => "contentPrj"
      case _           => "content"
    }

    val bodyContent = new MultiLang[String] {

      def ger = s"""
<div class="headline">
<span class="headline1">KIMI LUM</span>
<span class="headline2">ÜBERSETZERIN</span>
<span class="headline3">aus dem Deutschen ins Englische</span> 
${langMenu(currentPage, lang, menuLangList)}
</div>
${infoMenu(currentPage, menuPages, lang)}
<div id="${contentId}" style="">
$pageContent
</div>
      """

      def eng = s"""
<div class="headline">
<span class="headline1">KIMI LUM</span>
<span class="headline2">TRANSLATOR</span>
<span class="headline3">from German to English</span> 
${langMenu(currentPage, lang, menuLangList)}
</div>
${infoMenu(currentPage, menuPages, lang)}
<div id="${contentId}" style="">
$pageContent
</div>
      """

    }
    bodyTemplate(bodyContent.value(lang), currentPage, lang)
  }

  def projectToPage(prj: Project, pages: List[Page], languages: List[Lang]): Page = {
    new Page {
      val self = this
      def id = prj.id
      def name = new MultiLang[PageName] {
        def ger = PageName(None, prj.title.value(Ger).name.get)
        def eng = PageName(None, prj.title.value(Eng).name.get)
      }
      def menuSortOrder = 40
      def pageType = ProjectPage

      val isbn = if (prj.isbn.isDefined)
        s"""${prj.isbn.get}"""
      else ""

      def artist(lang: Lang): String = ArtistRolesUtil.format(prj.artist, lang).mkString("</br>")

      def beitrag(lang: Lang): String = prj.contrib match {
        case None    => ""
        case Some(a) => "| %s" format a.value(lang)
      }

      def yearString: String = prj.year match {
        case None    => " "
        case Some(y) => ", " + y.trim()
      }

      def comp(lang: Lang) = if (prj.company.isDefined)
        s"""${prj.company.get.typ.name.value(lang)}: ${prj.company.get.name}"""
      else ""

      def credit(lang: Lang) = Photocredit.credit(lang, prj.images.imageInfo(lang, ILProject).name) match {
        case Some(c) => c.trim()
        case None    => ""
      }

      def details(lang: Lang): String = prj.projectDetails match {
        case None =>
          val mdt = isbn match {
            case "" => s"""
${artist(lang)}

${comp(lang)}${yearString}  
${credit(lang)}
""".trim()
            case _ => s"""
${artist(lang)}

${comp(lang)}${yearString}  
${isbn}   
${credit(lang)}
""".trim()
          }
          Md.transf(mdt.trim())
        case Some(txt) =>
          val mdt = txt.value(lang)
          val re = Md.transf(mdt)
          re
      }

      def subTitle(lang: Lang): String = {
        prj.subTitle match {
          case None => ""
          case Some(st) =>
            s""" / ${st.value(lang)}"""
        }
      }

      def paddingLeft(lang: Lang): Int = prj.images.imageInfo(lang, ILProject).width + 35

      def subtitleHtml(lang: Lang): String = {

        prj.subTitle match {
          case None => ""
          case Some(st) => s"""
<div style="padding-left:${paddingLeft(lang)}px" class="prjSubtitle">${st.value(lang)}</div>
            """
        }

      }

      def content(lang: Lang): String =
        if (SpecialProjectPage.isSpecialPage(prj.id)) SpecialProjectPage.specialPage(id, lang)
        else contentDefault(lang)

      def contentDefault(lang: Lang): String = s"""
<img class="prjImg" src="images/${prj.images.imageInfo(lang, ILProject).name}"/>  
<div class="prjBody">
<div class="prjTitle">${prj.title.value(lang).name.getUqual.toUpperCase()} ${beitrag(lang)}</div>
${subtitleHtml(lang)}
<div style="padding-left:${paddingLeft(lang)}px" class="prjText">
${details(lang)}
</div>
            """

      def htmlText = new MultiLangSimple[String] {

        override def value(lang: Lang) = pageTemplate(self, pages, lang, languages,
          content(lang))
      }
    }

  }

  private def menuItemClass(loc: Location, active: Boolean): String =
    if (active) "menuItemF"
    else loc match {
      case LOC_First  => "menuItemF"
      case LOC_Middle => "menuItemM"
      case LOC_Last   => "menuItemL"
    }

  private def menuLangItemClass(loc: Location): String = loc match {
    case LOC_First  => "menuLangItemF"
    case LOC_Middle => "menuLangItemM"
    case LOC_Last   => "menuLangItemL"
  }

  def langMenu(current: Page, currentLang: Lang, languages: List[MenuLang]): String = {
    val oredered = languages.sortBy(_.lang.menuSortOrder)
    val li = oredered.map(l => {
      val clazz = menuLangItemClass(l.location)
      if (l.lang != currentLang) s"""<span class="$clazz"><a href="${fileName(current, l.lang)}">${l.lang.name}</a></span>"""
      else s"""<span class="$clazz">${l.lang.name}</span>"""
    })
    val m = li.mkString("")
    s"""<div class="langMenu">$m</div>"""
  }

  private def infoMenu(current: Page, relevant: List[MenuPage], lang: Lang): String = {
    val li = relevant.map(menuPage => {
      val active = current.id == menuPage.page.id
      val clazz = menuItemClass(menuPage.location, active)
      val idAttr = menuPage.id match {
        case None     => ""
        case Some(id) => s"""id="$id" """
      }
      val style = if (current.pageType != ProjectPage || menuPage.id.isEmpty || menuPage.id.get != "prjAlphaPage") ""
      else """ style="padding-bottom: 600px" """

      if (menuPage.page != current) s"""<div $idAttr $style class="$clazz"><a href="${fileName(menuPage.page, lang)}">${pageNameHtml(menuPage.page.name.value(lang))}</a></div>"""
      else s"""<div $idAttr $style class="$clazz">${pageNameHtml(menuPage.page.name.value(lang))}</div>"""
    })
    val m = li.mkString("")
    s"""<div id="infoMenu"><div class="menuItemFill"></div>$m</div>"""
  }

  def fileName(page: Page, lang: Lang): String = "%s-%s.html" format (page.id, lang.id)

  def fileName(prj: Project, lang: Lang): String = "%s-%s.html" format (prj.id, lang.id)

}

case object Homepage {

  import Implementation._
  import HtmlTemplateEngine._

  val outDir = new File("../output")

  val languages = List(Ger, Eng)

  val categories = List(Cat_A, Cat_D, Cat_M, Cat_L, Cat_S, Cat_U)

  val projects = ProjectFromFile.projects

  case object PagesConf {
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
            case x   => throw new IllegalStateException("Illegal hompageAlph character '%s'" format x)
          }
          re
        }

        def images(placements: List[ImagePlacement], lang: Lang): String = {
          val imgDivs = placements.map { p =>
            val startImageTextClass = p.alignment match {
              case TA_LeftTop     => "startImageTextL"
              case TA_RightTop    => "startImageTextR"
              case TA_LeftBottom  => "startImageTextL"
              case TA_RightBottom => "startImageTextR"
            }
            val title = p.prj.startTitle.value(lang)
            val contrib = p.prj.contrib match {
              case None    => ""
              case Some(c) => " | %s" format c.value(lang)
            }
            val credit = Photocredit.credit(lang, p.imgName) match {
              case None    => ""
              case Some(c) => "%s" format c
            }
            def imgHeight = p.prj.images.imageInfo(lang, ILStart).height - 1
            def imgWidth = p.prj.images.imageInfo(lang, ILStart).width - 1
            def imageDiv: String = s"""
<div class="startImageImage" style="background-image: url('images/${p.prj.images.imageInfo(lang, ILStart).name}'); width: ${imgWidth}px; height: ${imgHeight}px; " ></div>          
""".trim

            def imageTitleDiv: String = s"""
<div class="$startImageTextClass">${title}${contrib}</div>
""".trim

            def imageCreditDiv: String = s"""
<div class="$startImageTextClass">${credit}</div>
""".trim

            def imageAllDiv: String = p.alignment match {
              case TA_LeftTop     => imageTitleDiv + imageDiv + imageCreditDiv
              case TA_RightTop    => imageTitleDiv + imageDiv + imageCreditDiv
              case TA_LeftBottom  => imageCreditDiv + imageDiv + imageTitleDiv
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

        def links(artist: Artist1): String = {
          val prjs = projects.filter { p =>
            (p.artist.foldRight(false)((a, b) => b || a.name == artist.name)) && // What is that condition good for ??
              artist.role != AR1_Undef // Ignore artists with undefined role
          }
          val prjsSort = prjs.sortBy { x => x.title.value(lang).name.alph }
          prjsSort.map(p => {
            val page = projectToPage(p, pages, languages)
            s"""<div class="prjCatLink"><a href="${fileName(page, lang)}">${pageNameContentHtml(page.name.value(lang))}</a></div>"""
          }).mkString("")
        }

        val usedAuthors = projects.flatMap(p => p.artist).toSet.toList
        val ordered = usedAuthors.sortBy { _.name.toUpperCase() }

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
    val pages = List(startPage, bioContactPage, prjAlphaPage, prjCatPage, prjAuthPage)
  }

  def build(): Unit = {

    val pages = PagesConf.pages

    def build(p: Page, lang: Lang): Report = {

      def openFile(outDir: File, fname: String): PrintWriter = {
        if (!outDir.exists()) outDir.mkdirs()
        new PrintWriter(new File(outDir, fname), "UTF-8")
      }

      val fname = fileName(p, lang)
      val pw: PrintWriter = openFile(outDir, fname)
      try {
        pw.println(p.htmlText.value(lang))
      } finally {
        pw.close()
      }
      new Report {
        def heading = None
        def body = List.empty[ReportBody]
      }
    }

    def validateHompageAlpha(projects: List[Project]): CreationResult[Boolean] = {
      var infos = List.empty[String]
      var ok = true
      case class ProjectsForAlph(alph: Char, projectsIDsWithHpAlph: List[String])
      def projectsWithHpAlph(alph: Char, ps: List[Project], projectTitle: Project => ProjectTitle): List[String] = ps match {
        case Nil => Nil
        case p :: rest =>
          val tit = projectTitle(p)
          val al = tit.homepageAlph
          if (al.isDefined && al.get.toUpper == alph.toUpper) {
            p.id :: projectsWithHpAlph(alph, rest, projectTitle)
          } else {
            projectsWithHpAlph(alph, rest, projectTitle)
          }
      }
      val ger = 'A' to 'Z' map (char => ProjectsForAlph(char, projectsWithHpAlph(char, projects, (p: Project) => p.title.ger)))
      val eng = 'A' to 'Z' map (char => ProjectsForAlph(char, projectsWithHpAlph(char, projects, (p: Project) => p.title.eng)))
      ger.foreach(a => {
        if (a.projectsIDsWithHpAlph.size > 1) {
          ok = false
          infos ::= "'%s' is defined in more than one project for german. %s" format (a.alph, a.projectsIDsWithHpAlph.mkString(","))
        } else if (a.projectsIDsWithHpAlph.isEmpty) {
          infos ::= "'%s' is not defined for german" format (a.alph)
        }
      })
      eng.foreach(a => {
        if (a.projectsIDsWithHpAlph.size > 1) {
          ok = false
          infos ::= "'%s' is defined in more than one project for english. %s" format (a.alph, a.projectsIDsWithHpAlph.mkString(","))
        } else if (a.projectsIDsWithHpAlph.isEmpty) {
          infos ::= "'%s' is not defined for english" format (a.alph)
        }
      })

      new CreationResult[Boolean] {
        def result = ok
        def report = new Report {
          def heading = if (ok) Some("Homepage Alpha Validation OK") else Some("ERROR: Homepage Alpha Validation. More than one Project for a Character")
          def body = List(ReportLinesImpl(infos.reverse))
        }
      }
    }
    def buildPages: Report = {
      var buildReports = List.empty[ReportBody]
      List(Ger, Eng).foreach(lang => {
        pages.foreach(p => build(p, lang))
        projects.result.foreach(p => {
          buildReports ::= build(projectToPage(p, pages, languages), lang)
        })
      })
      new Report {
        def heading = Some("Built pages")
        def body = buildReports.reverse
      }
    }

    val validateHompageAlphaResult = validateHompageAlpha(projects.result)
    if (!validateHompageAlphaResult.result) {
      val rep = new Report {
        def heading = Some("Kimi Homepage. ERROR: No homepage created")
        def body = List(projects.report, validateHompageAlphaResult.report)
      }
      ReportFormatterTxt.format(rep)
    } else {
      val buildReport = buildPages
      ResCopy.copy(new File("src/main/web"), outDir, List("original", """\.DS.*"""))
      val rep = new Report {
        def heading = Some("Success Kimi Homepage")
        def body = List(validateHompageAlphaResult.report, projects.report, buildReport)
      }
      ReportFormatterTxt.format(rep)
    }
  }

}