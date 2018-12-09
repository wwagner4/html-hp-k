package net.entelijan.kimi

object Model {

  sealed trait ReportBody

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

  sealed trait ArtistRole {
    def masculineSingular: MultiLang[String]

    def femininSingular: MultiLang[String]

    def masculinePlural: MultiLang[String]

    def femininPlural: MultiLang[String]

    def mixedPlural: MultiLang[String]

    def order: Int
  }

  case object AR_Director$ extends ArtistRole {
    def masculineSingular = MultiLangString(ger = "Regie:", eng = "Director:")

    def femininSingular = MultiLangString(ger = "Regie:", eng = "Director:")

    def masculinePlural = MultiLangString(ger = "Regie:", eng = "Directors:")

    def femininPlural = MultiLangString(ger = "Regie:", eng = "Directors:")

    def mixedPlural = MultiLangString(ger = "Regie:", eng = "Directors:")

    def order = 50
  }

  case object AR_Editor extends ArtistRole {
    def masculineSingular = MultiLangString(ger = "Herausgeber:", eng = "Editor:")

    def femininSingular = MultiLangString(ger = "Herausgeberin:", eng = "Editor:")

    def masculinePlural = MultiLangString(ger = "Herausgeber:", eng = "Editors:")

    def femininPlural = MultiLangString(ger = "Herausgeberinnen:", eng = "Editors:")

    def mixedPlural = MultiLangString(ger = "Herausgegeben von", eng = "Editors:")

    def order = 40
  }

  case object AR_Composer extends ArtistRole {
    def masculineSingular = MultiLangString(ger = "Komponist:", eng = "Composer:")

    def femininSingular = MultiLangString(ger = "Komponistin:", eng = "Composer:")

    def masculinePlural = MultiLangString(ger = "Komponisten:", eng = "Composers:")

    def femininPlural = MultiLangString(ger = "Komponistinnen:", eng = "Composers:")

    def mixedPlural = MultiLangString(ger = "Komponisten von", eng = "Composers:")

    def order = 25
  }

  case object AR_Author extends ArtistRole {
    def masculineSingular = MultiLangString(ger = "Autor:", eng = "Author:")

    def femininSingular = MultiLangString(ger = "Autorin:", eng = "Author:")

    def masculinePlural = MultiLangString(ger = "Autoren:", eng = "Authors:")

    def femininPlural = MultiLangString(ger = "Autorinnen:", eng = "Authors:")

    def mixedPlural = MultiLangString(ger = "Autoren:", eng = "Authors:")

    def order = 20
  }

  case object AR_Artist extends ArtistRole {
    def masculineSingular = MultiLangString(ger = "Künstler:", eng = "Artist:")

    def femininSingular = MultiLangString(ger = "Künstlerin:", eng = "Artist:")

    def masculinePlural = MultiLangString(ger = "Künstler:", eng = "Artists:")

    def femininPlural = MultiLangString(ger = "Künstlerinnen:", eng = "Artists:")

    def mixedPlural = MultiLangString(ger = "Künstler:", eng = "Artists:")

    def order = 30
  }

  case object AR_Architect extends ArtistRole {
    def masculineSingular = MultiLangString(ger = "Architekt:", eng = "Architect:")

    def femininSingular = MultiLangString(ger = "Architektin:", eng = "Architect:")

    def masculinePlural = MultiLangString(ger = "Architekten:", eng = "Architects:")

    def femininPlural = MultiLangString(ger = "Architektinnen:", eng = "Architects:")

    def mixedPlural = MultiLangString(ger = "Architekten:", eng = "Architects:")

    def order = 10
  }

  case object AR_Undef extends ArtistRole {
    def masculineSingular = MultiLangString(ger = "", eng = "")

    def femininSingular = MultiLangString(ger = "", eng = "")

    def masculinePlural = MultiLangString(ger = "", eng = "")

    def femininPlural = MultiLangString(ger = "", eng = "")

    def mixedPlural = MultiLangString(ger = "", eng = "")

    def order = 11
  }

  case class Artist(gender: Gender, role: ArtistRole, name: String, nameReverse: String) {
    override def equals(o: Any): Boolean = o match {
      case that: Artist => that.name.equalsIgnoreCase(this.name)
      case _ => false
    }

    override def hashCode: Int = 1
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
    def alph: String = get match {
      case "" => "ZZZZZZ"
      case _ => get.trim().toUpperCase()
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
    def ger: String = value

    def eng: String = value
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
    def parse(roles: String, names: String, namesRev: String): List[Artist] = {
      def roleNameToArtist(rn: (String, (String, String))): Artist = {

        def parseGender(code: String): Gender =
          if (code.startsWith("fem")) G_Feminin
          else if (code.startsWith("mas")) G_Masculin
          else G_Undef

        def parseRole(code: String): ArtistRole =
          if (code.endsWith("D")) AR_Director$
          else if (code.endsWith("E")) AR_Editor
          else if (code.endsWith("W")) AR_Author
          else if (code.endsWith("A")) AR_Artist
          else if (code.endsWith("B")) AR_Architect
          else if (code.endsWith("K")) AR_Composer
          else AR_Undef

        val code = rn._1.trim()

        def gender = parseGender(code)

        def role = parseRole(code)

        def name = rn._2._1.trim()

        def nameRev = rn._2._2.trim()

        Artist(gender, role, name, nameRev)
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

    def format(artists: List[Artist], lang: Lang): List[String] = {
      def roleGroupsToString(rg: List[Artist], lang: Lang): String = {
        val g = rg(0)

        def allFeminin: Boolean = rg.forall(artist => artist.gender == G_Feminin)

        def allMasculin: Boolean = rg.forall(artist => artist.gender == G_Masculin)

        def heading: String =
          if (rg.size == 1) {
            rg(0).gender match {
              case G_Feminin => g.role.femininSingular.value(lang)
              case G_Masculin => g.role.masculineSingular.value(lang)
              case G_Undef => ""
            }
          } else if (allFeminin) g.role.femininPlural.value(lang)
          else if (allMasculin) g.role.masculinePlural.value(lang)
          else g.role.mixedPlural.value(lang)

        def names = rg.map {
          _.nameReverse
        }.mkString(", ")

        "%s %s" format(heading, names)

      }

      if (artists.isEmpty) throw new IllegalStateException("Artists list must never be empty")
      val roles = artists.groupBy { x => x.role }
      roles.values.toList.map(rg => roleGroupsToString(rg, lang))
    }

  }

  sealed trait Device

  case object DV_Browser extends Device

  case object DV_MobilePortrait extends Device

  case object DV_MobileLandscape extends Device


  case class DeviceData[T] (device: Device, data: T)


  sealed trait PageType

  case object PT_Start extends PageType

  case object PT_ProjectOverview extends PageType

  case object PT_Project extends PageType

  case object PT_Info extends PageType


  case class PageName(prefix: Option[String], name: String)

  trait Page {
    def id: String

    def name: MultiLang[PageName]

    def pageType: PageType

    def menuSortOrder: Int

    def menuId: Option[String]

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

    def artist: List[Artist]

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
            case EngPrjRegex() => NameAnalyzed(Some(Eng), Some(ILProject), imgInfo)
            case GerPrjRegex() => NameAnalyzed(Some(Ger), Some(ILProject), imgInfo)
            case GerRegex() => NameAnalyzed(Some(Ger), None, imgInfo)
            case EngRegex() => NameAnalyzed(Some(Eng), None, imgInfo)
            case StartRegex() => NameAnalyzed(None, Some(ILStart), imgInfo)
            case PrjRegex() => NameAnalyzed(None, Some(ILProject), imgInfo)
            case AnyRegex() => NameAnalyzed(None, None, imgInfo)
          }
        }
      }

      require(files.size > 0)

      val analyzed = analyze(files)

      new ImageLangLoc {

        lazy val gerStart: ImageInfo = bestFit(Ger, ILStart, analyzed)
        lazy val engStart: ImageInfo = bestFit(Eng, ILStart, analyzed)
        lazy val gerPrj: ImageInfo = bestFit(Ger, ILProject, analyzed)
        lazy val engPrj: ImageInfo = bestFit(Eng, ILProject, analyzed)

        def imageInfo(lang: Lang, loc: ImageLocation): ImageInfo = {
          lang match {
            case Ger => loc match {
              case ILStart => gerStart
              case ILProject => gerPrj
            }
            case Eng => loc match {
              case ILStart => engStart
              case ILProject => engPrj
            }
          }
        }
      }
    }
  }

}