package net.entelijan.kimi

import java.io._

import net.entelijan.kimi.PagesConf._
import net.entelijan.kimi.renderer.MobileRenderer01

case object Homepage {

  import HtmlTemplateEngine._
  import Model._

  private val outDir = KimiUtil.outputDir

  private val languages = List(Ger, Eng)

  private val devices = List(DV_Browser, DV_MobilePortrait, DV_MobileLandscape)

  private val projects: CreationResult[List[Project]] = ProjectFromFile.projects

  lazy val renderer: Renderer = new MobileRenderer01

  private val startPage: Page = StartPage(projects.result)
  private val prjAlphaPage: Page = PrjAlphaPage(projects.result)
  private val prjCatPage: Page = PrjCatPage(projects.result)
  private val prjAuthPage: Page = PrjAuthPage(projects.result)
  private val bioContactPage: Page = BioContactPage()

  val pages: List[Page] = List(startPage, bioContactPage, prjAlphaPage, prjCatPage, prjAuthPage)

  def build(): Unit = {

    val validateHompageAlphaResult = validateHompageAlpha(projects.result)
    if (!validateHompageAlphaResult.result) {
      val rep = new Report {
        def heading = Some("Kimi Homepage. ERROR: No homepage created")

        def body = List(projects.report, validateHompageAlphaResult.report)
      }
      ReportFormatterTxt.format(rep)
    } else {
      val buildReport = buildPages(pages)
      ResCopy.copy(new File("src/main/web"), outDir, List("original", """\.DS.*"""))

      val allReports: List[Report] = List(validateHompageAlphaResult.report, projects.report, buildReport)
      val rep = new Report {
        def heading: Some[String] =
          if (containsErrorReports(allReports)) Some("ERROR creating Kimi Homepage")
          else Some("Success creating Kimi Homepage")

        def body: List[Report] = allReports
      }
      ReportFormatterTxt.format(rep)
    }
  }

  private def validateHompageAlpha(projects: List[Project]): CreationResult[Boolean] = {
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
        infos ::= "'%s' is defined in more than one project for german. %s" format(a.alph, a.projectsIDsWithHpAlph.mkString(","))
      } else if (a.projectsIDsWithHpAlph.isEmpty) {
        infos ::= "'%s' is not defined for german" format a.alph
      }
    })
    eng.foreach(a => {
      if (a.projectsIDsWithHpAlph.size > 1) {
        ok = false
        infos ::= "'%s' is defined in more than one project for english. %s" format(a.alph, a.projectsIDsWithHpAlph.mkString(","))
      } else if (a.projectsIDsWithHpAlph.isEmpty) {
        infos ::= "'%s' is not defined for english" format a.alph
      }
    })

    new CreationResult[Boolean] {
      def result: Boolean = ok

      def report: Report = new Report {
        def heading: Some[String] = if (ok) Some("Homepage Alpha Validation OK") else Some("ERROR: Homepage Alpha Validation. More than one Project for a Character")

        def body: List[ReportLinesImpl] = List(ReportLinesImpl(infos.reverse))
      }
    }

  }

  private def buildPages(pages: List[Page]): Report = {
    var buildReports = List.empty[ReportBody]
    languages.foreach { lang =>
      pages.foreach(p => buildPage(p, lang))
      projects.result.foreach { p =>
        buildReports ::= buildPage(ProjectPage(p), lang)
      }
    }
    new Report {

      def heading: Option[String] = Some("Built pages")

      def body: List[ReportBody] = buildReports.reverse
    }
  }


  private def buildPage(page: Page, lang: Lang): Report = {

    def openFile(outDir: File, fname: String): PrintWriter = {
      if (!outDir.exists()) outDir.mkdirs()
      new PrintWriter(new File(outDir, fname), "UTF-8")
    }

    val fname = fileName(page, lang)
    val pw: PrintWriter = openFile(outDir, fname)
    try {
      pw.println(renderer.rendPage(page, pages, lang, languages, devices))
    } finally {
      pw.close()
    }
    new Report {
      def heading = Some(s"Created output for page ${page.id} $outDir/$fname")

      def body = List.empty[ReportBody]
    }
  }

  private def containsErrorReports(reports: List[Report]): Boolean = {

    def containsErrorReport(report: Report): Boolean = {
      if (report.heading.isDefined && report.heading.get.contains("ERROR")) true
      else {
        if (containsErrorBodies(report.body)) true
        else false
      }
    }

    def containsErrorBodies(bodies: List[ReportBody]): Boolean = {
      bodies match {
        case Nil => false
        case (body: ReportLines) :: _ => body.lines.exists(s => s.contains("ERROR"))
        case (report: Report) :: rest =>
          if (containsErrorReport(report)) true
          else containsErrorBodies(rest)

      }
    }

    reports match {
      case Nil => false
      case r :: rest =>
        if (containsErrorReport(r)) true
        else containsErrorReports(rest)

    }
  }

}