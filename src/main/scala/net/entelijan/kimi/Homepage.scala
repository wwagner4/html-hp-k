package net.entelijan.kimi

import java.io._

case object Homepage {

  import HtmlTemplateEngine._
  import Model._

  val outDir = KimiUtil.outputDir

  val languages = List(Ger, Eng)

  val categories = List(Cat_A, Cat_D, Cat_M, Cat_L, Cat_S, Cat_U)

  val projects: CreationResult[List[Project]] = ProjectFromFile.projects

  def build(): Unit = {

    val validateHompageAlphaResult = validateHompageAlpha(projects.result)
    if (!validateHompageAlphaResult.result) {
      val rep = new Report {
        def heading = Some("Kimi Homepage. ERROR: No homepage created")

        def body = List(projects.report, validateHompageAlphaResult.report)
      }
      ReportFormatterTxt.format(rep)
    } else {
      val buildReport = buildPages(PagesConf.pages)
      ResCopy.copy(new File("src/main/web"), outDir, List("original", """\.DS.*"""))

      val allReports: List[Report] = List(validateHompageAlphaResult.report, projects.report, buildReport)
      val rep = new Report {
        def heading =
          if (containsErrorReports(allReports)) Some("ERROR creating Kimi Homepage")
          else Some("Success creating Kimi Homepage")

        def body = allReports
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
        infos ::= "'%s' is not defined for german" format (a.alph)
      }
    })
    eng.foreach(a => {
      if (a.projectsIDsWithHpAlph.size > 1) {
        ok = false
        infos ::= "'%s' is defined in more than one project for english. %s" format(a.alph, a.projectsIDsWithHpAlph.mkString(","))
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

  private def buildPages(pages: List[Page]): Report = {
    var buildReports = List.empty[ReportBody]
    List(Ger, Eng).foreach(lang => {
      pages.foreach(p => buildPage(p, lang))
      projects.result.foreach(p => {
        buildReports ::= buildPage(projectToPage(p, pages, languages), lang)
      })
    })
    new Report {
      def heading = Some("Built pages")

      def body = buildReports.reverse
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
      pw.println(page.htmlText.value(lang))
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
        case (body: ReportLines) :: rest => body.lines.exists(s => s.contains("ERROR"))
        case (report: Report) :: rest => {
          if (containsErrorReport(report)) true
          else containsErrorBodies(rest)
        }
      }
    }

    reports match {
      case Nil => false
      case r :: rest => {
        if (containsErrorReport(r)) true
        else containsErrorReports(rest)
      }
    }
  }

}