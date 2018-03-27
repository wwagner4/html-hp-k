package net.entelijan.kimi

object HtmlTemplateEngine {

  import Model._

  def pageNameHtml(name: PageName): String = name.prefix match {
    case None => s"""<span class="infoMenuItemText">${name.name.toUpperCase()}</span>"""
    case Some(prefix) => s"""<span class="infoMenuItemTextPrefix">$prefix</span><span class="infoMenuItemText">${name.name.toUpperCase()}</span>"""
  }

  def pageNameContentHtml(name: PageName): String = name.prefix match {
    case None => s"""${name.name}"""
    case Some(prefix) => s"""$prefix ${name.name}"""
  }


  sealed trait Location

  case object LOC_First extends Location

  case object LOC_Middle extends Location

  case object LOC_Last extends Location

  case class MenuPage(page: Page, location: Location, id: Option[String])

  case class MenuLang(lang: Lang, location: Location)

  def fileName(page: Page, lang: Lang): String = "%s-%s.html" format(page.id, lang.id)

  def fileName(prj: Project, lang: Lang): String = "%s-%s.html" format(prj.id, lang.id)

  def pageTemplate(bodyContent: List[DeviceData[String]], page: Page, lang: Lang): String = {


    val mobileMaxWidth = 900

    val reloadScript =
      if (page.pageType == PT_Start)
        """<script type="text/javascript">
          |function doPageReload() {
          |    if(isMobile()) {
          |      // Nothing to do
          |    } else {
          |      window.location.href = window.location.href;
          |    }
          |};
          |var resizeTimer;
          |$(window).resize(function() {
          |    if (!isMobile()) {
          |      clearTimeout(resizeTimer);
          |      resizeTimer = setTimeout(doPageReload, 100);
          |    }
          |});
          |</script>
          |""".stripMargin
      else ""

    val alignContentScript =
      if (page.pageType == PT_ProjectOverview || page.pageType == PT_Info)
        s"""<script type="text/javascript">
           |function doAlignContent() {
           |    // console.log("doAlignContent");
           |    var mi = $$("#${page.menuId.get}");
           |    var x = mi.offset().left;
           |    var y = mi.offset().top;
           |    var h = mi.height();
           |    //console.log("#${page.menuId.get} " + x + " " + y + " " + h);
           |    var c = $$("#content");
           |    c.css("left", "" + x + "px");
           |    c.css("top", "" + y + "px");
           |    var m = $$("#infoMenu")
           |    if (m.width() >= 1200) c.width(m.width() - 40)
           |    else c.width(m.width() - x - 40)
           |};
           |$$(window).load(function() {
           |  doAlignContent();
           |});
           |$$(document).ready(function() {
           |  doAlignContent();
           |});
           |$$(window).resize(function() {
           |  if (!isMobile()) {
           |    doAlignContent();
           |  }
           |});
           |</script>
           |""".stripMargin
      else ""

    val alignPrjPageScript =
      if (page.pageType == PT_Project)
        s"""<script type="text/javascript">
           |function doAlignPrjPage() {
           |    var pa = $$("#page");
           |    //console.log("pa " + pa);
           |    if (pa.offset()) {
           |      // console.log("pa offset " + pa.offset());
           |      var y = pa.offset().left + 60;
           |      // console.log("prj offset " + y);
           |      var c = $$("#contentPrj");
           |      c.css("left", "" + y + "px");
           |    } else {
           |      // console.log("pa offset NOT READY");
           |    }
           |};
           |$$(window).load(function() {
           |  doAlignPrjPage();
           |});
           |$$(document).ready(function() {
           |  doAlignPrjPage();
           |});
           |$$(window).resize(function() {
           |  if (!isMobile()) {
           |    doAlignPrjPage();
           |  }
           |});
           |</script>
           |""".stripMargin
      else ""

    s"""<!DOCTYPE html>
       |<html>
       |<head>
       |<meta charset="UTF-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1.0">
       |<title>Kimi Lum ${page.name.value(lang).name}</title>
       |<style type="text/css">
       |${Css.all}
       |</style>
       |<style media="(max-width: ${mobileMaxWidth}px) and (orientation: portrait)" type="text/css">
       |${Css.mobilePortrait}
       |</style>
       |<style media="(max-width: ${mobileMaxWidth}px) and (orientation: landscape)" type="text/css">
       |${Css.mobileLandscape}
       |</style>
       |<script type="text/javascript" src="js/jquery-1.11.1.min.js"></script>
       |<script type="text/javascript">
       |function isMobile() {
       |    return /Edge|Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
       |}
       |</script>
       |$reloadScript
       |$alignContentScript
       |$alignPrjPageScript
       |</head>
       |<body>
       |${deviceContent(bodyContent)}
       |</body>
       |</html>
       |""".stripMargin
  }

  def deviceContent(data: List[DeviceData[String]]): String = {

    def cont(d: DeviceData[String]): String =
      s"""<div class="${d.device}">
         |   <div id="page">
         |   ${d.data}
         |   </div>
         |</div>
         |""".stripMargin

     data.map(cont).mkString("\n")

  }

  def locations(size: Int): List[Location] = {
    (1 to size).toList map { i =>
      if (i == 1) LOC_First
      else if (i >= size) LOC_Last
      else LOC_Middle
    }
  }

  def menuPages(pages: List[Page]): List[MenuPage] = {
    def convertToMenuPages(pages: List[Page]): List[MenuPage] = {
      val locs = locations(pages.size)
      pages.zip(locs) map { case (page, loc) => MenuPage(page, loc, page.menuId) }
    }
    val relPages = pages
      .filter { page =>
        page.pageType == PT_Start || page.pageType == PT_ProjectOverview || page.pageType == PT_Info }
      .sortBy(_.menuSortOrder)
    convertToMenuPages(relPages)
  }

  def menuLangList(languages: List[Lang]): List[MenuLang] = {
    val locs = locations(languages.size)
    languages.zip(locs) map { case (a, b) => MenuLang(a, b) }
  }



}