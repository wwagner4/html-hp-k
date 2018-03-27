package net.entelijan.kimi

import net.entelijan.kimi.Model._

object PagesConf {

  case class StartPage(projects: List[Project]) extends Page {

    override def id: String = "index"

    override def name: MultiLang[PageName] =
      MultiLangGeneric(
        PageName(None, "START"),
        PageName(None, "HOME"))

    override def menuId: Option[String] = None

    override def pageType: PageType = PT_Start

    override def menuSortOrder: Int = 0

  }

  case class PrjAlphaPage(projects: List[Project]) extends Page {

    override def id: String = "prjAlphaPage"

    override def name: MultiLang[PageName] =
      MultiLangGeneric(
        PageName(Some("Projekte nach"), "TITEL"),
        PageName(Some("Projects by"), "TITLE"))

    override def menuSortOrder: Int = 10

    override def menuId: Option[String] = Some(id)

    override def pageType: PageType = PT_ProjectOverview

  }

  case class PrjAuthPage(projects: List[Project]) extends Page {

    override def id: String = "prjAuthPage"

    override def name: MultiLang[PageName] = MultiLangGeneric(
      PageName(Some("Projekte nach"), "AUTOR"),
      PageName(Some("Projects by"), "AUTHOR"))

    override def menuSortOrder: Int = 15

    override def menuId: Option[String] = Some(id)

    override def pageType: PageType = PT_ProjectOverview

  }

  case class PrjCatPage(projects: List[Project]) extends Page {

    override def id: String = "prjCatPage"

    override def name: MultiLang[PageName] = MultiLangGeneric(PageName(Some("Projekte nach"), "KATEGORIE"), PageName(Some("Projects by"), "CATEGORY"))

    override def menuSortOrder: Int = 20

    override def menuId = Some(id)

    override def pageType: PageType = PT_ProjectOverview

  }

  case class BioContactPage() extends Page {

    override def id: String = "bio"

    override def name: MultiLang[PageName] =
      MultiLangGeneric(
        PageName(None, "LEBENSLAUF / KONTAKT"),
        PageName(None, "BIO / CONTACT")
      )

    override def menuSortOrder: Int = 60

    override def menuId: Option[String] = Some(id)

    override def pageType: PageType = PT_Info

  }

  case class ProjectPage(prj: Project) extends Page {

    override def id: String = prj.id

    override def name: MultiLang[PageName] =
      MultiLangGeneric(
        PageName(None, prj.title.value(Ger).name.get),
        PageName(None, prj.title.value(Eng).name.get)
      )

    override def menuSortOrder: Int = 40

    override def menuId: Option[String] = None

    override def pageType: PageType = PT_Project

  }


}
