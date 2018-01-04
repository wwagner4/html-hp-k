package net.entelijan.kimi

object HtmlTemplateEngine {

  import Model._

  def pageNameHtml(name: PageName): String = name.prefix match {
    case None => s"""<span class="infoMenuItemText">${name.name.toUpperCase()}</span>"""
    case Some(prefix) => s"""<span class="infoMenuItemTextPrefix">${prefix}</span><span class="infoMenuItemText">${name.name.toUpperCase()}</span>"""
  }

  def pageNameContentHtml(name: PageName): String = name.prefix match {
    case None => s"""${name.name}"""
    case Some(prefix) => s"""${prefix} ${name.name}"""
  }

  def bodyTemplate(bodyContent: String, page: Page, lang: Lang): String = {
    val bodyStyle =
      if (page.pageType == Start)
        """style="overflow:hidden;" """
      else ""

    val reloadScript =
      if (page.pageType == Start)
        """
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
      if (page.menuId.isDefined)
        s"""
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
      if (page.pageType == ProjectPage)
        s"""
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
      case Start => "contentStart"
      case ProjectPage => "contentPrj"
      case _ => "content"
    }

    val bodyContent = new MultiLang[String] {

      def ger =
        s"""
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

      def eng =
        s"""
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

      def htmlText = new MultiLangSimple[String] {

        override def value(lang: Lang) = pageTemplate(self, pages, lang, languages,
          content(lang))
      }
    }

  }

  private def menuItemClass(loc: Location, active: Boolean): String =
    if (active) "menuItemF"
    else loc match {
      case LOC_First => "menuItemF"
      case LOC_Middle => "menuItemM"
      case LOC_Last => "menuItemL"
    }

  private def menuLangItemClass(loc: Location): String = loc match {
    case LOC_First => "menuLangItemF"
    case LOC_Middle => "menuLangItemM"
    case LOC_Last => "menuLangItemL"
  }

  def langMenu(current: Page, currentLang: Lang, languages: List[MenuLang]): String = {
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

  private def infoMenu(current: Page, relevant: List[MenuPage], lang: Lang): String = {
    val li = relevant.map(menuPage => {
      val active = current.id == menuPage.page.id
      val clazz = menuItemClass(menuPage.location, active)
      val idAttr = menuPage.id match {
        case None => ""
        case Some(id) => s"""id="$id" """
      }
      val style = if (current.pageType != ProjectPage || menuPage.id.isEmpty || menuPage.id.get != "prjAlphaPage") ""
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

  def fileName(page: Page, lang: Lang): String = "%s-%s.html" format(page.id, lang.id)

  def fileName(prj: Project, lang: Lang): String = "%s-%s.html" format(prj.id, lang.id)

}