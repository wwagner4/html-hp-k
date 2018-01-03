package net.entelijan.kimi

import net.entelijan.kimi.Implementation._
import java.io.FileInputStream
import scala.io.Codec
import scala.io.BufferedSource
import scala.annotation.tailrec
import java.beans.DesignMode
import java.io.File
import javax.imageio.ImageIO
import javax.imageio.ImageReader
import java.io.IOException
import javax.imageio.stream.ImageInputStream
import javax.imageio.stream.FileImageInputStream

object ProjectFromFile {

  val fileName = "../input/Projekte.csv"
  val colCount = 26

  def projects: CreationResult[List[Project]] = {
    var reps = List.empty[Report]
    val bs = scala.io.Source.fromFile(fileName)(Codec.UTF8)
    val lines = bs.getLines.toList
    val re = lines.zipWithIndex.flatMap {
      case (line, nr) => {
        val r = toProject(line, nr)
        reps ::= r.report
        r.result
      }
    }
    new CreationResult[List[Project]] {
      def report = new Report {
        def heading = Some("Successfully created %d projects" format re.size)
        def body = reps.reverse
      }
      def result = re
    }
  }

  def toProject(line: String, nr: Int): CreationResult[Option[Project]] = {
    try {
      val ori = line.split("\t").toList
      if (ori.size > colCount) {
        throw new IllegalStateException("Fehler in Zeile %d '%s'. Die Anzahl der Spalten ist nicht %d sondern %d" format (nr, line, colCount, ori.size))
      }
      val adjusted = if (ori.size < colCount) fill(ori, "", colCount) else ori
      toProject(adjusted, nr)
    } catch {
      case e: Exception => throw new IllegalArgumentException("Error parsing line %d '%s'. %s" format (nr, line, e.getMessage), e)
    }
  }

  @tailrec
  def fill(l: List[String], str: String, len: Int): List[String] = {
    if (l.size == len) l
    else fill(l ::: List(str), str, len)
  }

  def toProject(line: List[String], nr: Int): CreationResult[Option[Project]] = {
    var infos = List.empty[String]
    def createId: String = {
      val id = line(0)
      if (id == "") {
        throw new ProjectException("ERROR: No ID defined in line %d. %s" format (nr, line.mkString(";")))
      }
      id
    }

    def createYear: Option[String] = {
      val year = line(24)
      year match {
        case ""  => None
        case any => Some(any)
      }
    }

    def createProjectDetails: Option[MultiLang[String]] = {
      def linebreak(l: String): String = {
        val repl = List(' ', ' ', '\n').mkString("");
        l.replaceAll(":::", "\n\n").replaceAll("::", repl)
      }
      val pdDe = linebreak(line(20).trim())
      val pdEn = linebreak(line(21).trim())
      (pdDe, pdEn) match {
        case ("", "") => None
        case (x, "")  => Some(MultiLangStringSimple(x))
        case ("", x)  => Some(MultiLangStringSimple(x))
        case (de, en) => Some(MultiLangString(de, en))
      }
    }

    def createStartTitle: MultiLang[String] = {
      val startTitelGer = line(3)
      val startTitelEng = line(4)
      val _ger = if (startTitelGer == "") createTitle.ger.name.get else startTitelGer
      val _eng = if (startTitelEng == "") createTitle.eng.name.get else startTitelEng
      new MultiLangString(_ger, _eng)
    }

    def createTitle: MultiLang[ProjectTitle] = {
      val hpAlph = line(2).trim().toUpperCase()
      val alphDe = line(7).trim().toUpperCase()
      val namDe = line(8).trim()
      val alphEn = line(10).trim().toUpperCase()
      val namEn = line(11).trim()
      def createHomepageAlph(char: String): Option[Char] = char match {
        case ""  => None
        case any => Some(any.charAt(0))
      }
      def create(alphStringGer: AlphString, alphStringEng: AlphString): MultiLang[ProjectTitle] = {
        new MultiLang[ProjectTitle] {
          def ger = new ProjectTitle {
            override def homepageAlph = createHomepageAlph(hpAlph)
            def name = alphStringGer
          }
          def eng = new ProjectTitle {
            override def homepageAlph = createHomepageAlph(hpAlph)
            def name = alphStringEng
          }
        }
      }
      if (namDe == "" && namEn == "") {
        throw new ProjectException("Error in line %d. Neither english nor german name defined. %s" format (nr, line.mkString(";")))
      } else if (namDe != "" && namEn == "") {
        infos ::= "Info for line %d. No english title defined" format (nr)
        if (alphDe != "") create(AlphStringImpl(alphDe, namDe, uqual(namDe)), AlphStringImpl(alphDe, namDe, uqual(namDe)))
        else create(AlphStringSimple(namDe, uqual(namDe)), AlphStringSimple(namDe, uqual(namDe)))
      } else if (namDe == "" && namEn != "") {
        infos ::= "Info for line %d. No german title defined" format (nr)
        if (alphEn != "") create(AlphStringImpl(alphEn, namEn, uqual(namEn)), AlphStringImpl(alphEn, namEn, uqual(namEn)))
        else create(AlphStringSimple(namEn, uqual(namEn)), AlphStringSimple(namEn, uqual(namEn)))
      } else {
        val de = if (alphDe != "") AlphStringImpl(alphDe, namDe, uqual(namDe)) else AlphStringSimple(namDe, uqual(namDe))
        val en = if (alphEn != "") AlphStringImpl(alphEn, namEn, uqual(namEn)) else AlphStringSimple(namEn, uqual(namEn))
        create(de, en)
      }
    }
    def uqual(title: String): String = KimiUtil.uqual(title)
    def createSubTitle: Option[MultiLang[String]] = createMultilangString(9, 12)
    def createContrib: Option[MultiLang[String]] = createMultilangString(5, 6)

    def createMultilangString(indexDe: Int, indexEn: Int): Option[MultiLang[String]] = {
      val de = line(indexDe)
      val en = line(indexEn)
      if (de == "" && en == "") {
        None
      } else if (de != "" && en == "") {
        Some(MultiLangString(de, de))
      } else if (de == "" && en != "") {
        Some(MultiLangString(en, en))
      } else {
        Some(MultiLangString(de, en))
      }
    }
    def createArtistList: List[Artist1] = {
      val roles = line(16)
      val nams = line(18)
      val namsRev = line(19)
      ArtistRolesUtil.parse(roles, nams, namsRev)
    }

    def createCategory: List[Category] = {
      val code = line(13)
      val codeList = code.split("/").toList
      codeList map { c =>
        c.trim().toUpperCase() match {
          case "M" => Cat_M
          case "L" => Cat_L
          case "D" => Cat_D
          case "S" => Cat_S
          case "A" => Cat_A
          case "U" => Cat_U
          case x   => throw new ProjectException("Error in line %d. Unknown category code %s. %s" format (nr, x, line))
        }
      }
    }
    def createIsbn: Option[String] = {
      val code = line(25)
      code match {
        case ""  => None
        case any => Some(any)
      }
    }

    def createCompany: Option[Company] = {
      val typStr = line(22)
      val namStr = line(23)
      namStr match {
        case "" => None
        case any => Some(new Company {
          def name = namStr
          def typ = typStr match {
            case "R" => Comp_R
            case "P" => Comp_P
            case "M" => Comp_M
            case "E" => Comp_E
            case "L" => Comp_L
            case ""  => Comp_UNDEF
            case x   => throw new IllegalStateException("'%s' cannot be matched to a CompanyType (R, P, M, E, L or '')" format x)
          }
        })
      }
    }

    def createImageInfo(id: String): ImageLangLoc = {
      import java.awt.Dimension

      def createImageInfo(f: File): ImageInfo = {
        val n = f.getName
        val dim = getJPEGDimension(f)
        new ImageInfo {
          def name = n
          def width = dim.width
          def height = dim.height
        }
      }

      def getSuffix(f: File): String = {
        val path = f.getAbsolutePath
        val idx = path.lastIndexOf('.')
        if (idx < 0) throw new IllegalStateException("File %s has no suffix" format f)
        path.substring(idx + 1)
      }

      def getJPEGDimension(f: File): Dimension = {
        //println("img: %s" format f)
        val suff = getSuffix(f)
        val iter = ImageIO.getImageReadersBySuffix(suff);
        if (iter.hasNext()) {
          val reader: ImageReader = iter.next();
          try {
            val stream: ImageInputStream = new FileImageInputStream(f);
            reader.setInput(stream);
            val width = reader.getWidth(reader.getMinIndex());
            val height = reader.getHeight(reader.getMinIndex());
            new Dimension(width, height);
          } finally {
            reader.dispose();
          }
        } else {
          throw new IllegalStateException("Found no ImageReader for '%s'" format suff);
        }
      }

      val imgDir = new File("src/main/web/images");
      val imgFiles = imgDir.listFiles().toList.filter { f => f.isFile() && f.getName.startsWith(id) }
      if (imgFiles.isEmpty) {
        throw new ProjectException("Found no image for project: '%s'" format id)
      }
      val imgInfos = imgFiles.map { f => createImageInfo(f) }
      ImageLangLocFactory.toImageLangLoc(imgInfos)
    }

    try {
      val deactivated: Boolean = line(1).toLowerCase().startsWith("x")
      if (deactivated) {
        new CreationResult[Option[Project]] {
          def result = None
          def report = new Report {
            def heading = Some("INFO Project '%s' not active" format (line(0)))
            def body = List.empty[ReportBody]
          }
        }
      } else {
        new CreationResult[Option[Project]] {
          val _id = createId
          val prj = new Project {
            val id: String = _id
            val title: MultiLang[ProjectTitle] = createTitle
            val startTitle: MultiLang[String] = createStartTitle
            val subTitle: Option[MultiLang[String]] = createSubTitle
            val contrib: Option[MultiLang[String]] = createContrib
            val artist: List[Artist1] = createArtistList
            val category: List[Category] = createCategory
            val isbn: Option[String] = createIsbn
            val company: Option[Company] = createCompany
            val images: ImageLangLoc = createImageInfo(_id)
            override val year: Option[String] = createYear
            override val projectDetails = createProjectDetails
          }
          def result = Some(prj)
          def report = infos match {
            case Nil => new Report {
              def heading = Some("INFO Create project %s" format prj.id)
              def body = List.empty[ReportBody]
            }
            case is => new Report {
              def heading = Some("INFO Creating project %s" format prj.id)
              def body = List(ReportLinesImpl(infos))
            }
          }
        }
      }
    } catch {
      case e: ProjectException => {
        new CreationResult[Option[Project]] {
          def result = None
          def report = new Report {
            def heading = Some("ERROR creating project. %s" format e.getMessage())
            def body = List.empty[ReportBody]
          }
        }
      }
    }
  }
  class ProjectException(msg: String) extends RuntimeException(msg)
}