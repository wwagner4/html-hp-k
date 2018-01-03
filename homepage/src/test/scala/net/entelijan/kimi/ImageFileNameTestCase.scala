package net.entelijan.kimi

import org.scalatest._
import net.entelijan.kimi.Implementation._

class ImageFileNameTestCase extends FunSuite {

  case class T(nr: Int, files: List[String], gerStart: String, engStart: String, gerPrj: String, engPrj: String)

  case class II(name: String, width: Int, height: Int) extends ImageInfo
  
  
  val testvalues = List(
    T(1, List(
      "a_DE.jpg",
      "a_EN.jpg"),
      "a_DE.jpg", "a_EN.jpg", "a_DE.jpg", "a_EN.jpg"),
    T(2, List(
      "a_EN_START.jpg",
      "a_DE.jpg"),
      "a_DE.jpg", "a_EN_START.jpg", "a_DE.jpg", "a_EN_START.jpg"),
    T(3, List(
      "a_EN_START.jpg",
      "a_DE.jpg",
      "a_EN.jpg"),
      "a_DE.jpg", "a_EN_START.jpg", "a_DE.jpg", "a_EN.jpg"),
    T(4, List(
      "a_EN_START.jpg",
      "a_DE.jpg",
      "a_EN_PRJ.jpg",
      "a_EN.jpg"),
      "a_DE.jpg", "a_EN_START.jpg", "a_DE.jpg", "a_EN_PRJ.jpg"),
    T(5, List(
      "a_START.jpg",
      "a_PRJ.jpg"),
      "a_START.jpg", "a_START.jpg", "a_PRJ.jpg", "a_PRJ.jpg"),
    T(6, List(
      "a_EN_START.jpg",
      "a_EN_PRJ.jpg"),
      "a_EN_START.jpg", "a_EN_START.jpg", "a_EN_PRJ.jpg", "a_EN_PRJ.jpg"),
    T(7, List(
      "a_START.jpg"),
      "a_START.jpg", "a_START.jpg", "a_START.jpg", "a_START.jpg"),
    T(8, List(
      "a.jpg",
      "a_START.jpg"),
      "a_START.jpg", "a_START.jpg", "a.jpg", "a.jpg"),
    T(9, List(
      "a_PRJ.jpg",
      "a.jpg"),
      "a.jpg", "a.jpg", "a_PRJ.jpg", "a_PRJ.jpg"),
    T(10, List(
      "a.jpg"),
      "a.jpg", "a.jpg", "a.jpg", "a.jpg"),
    T(11, List(
      "a_DE_PRJ.jpg"),
      "a_DE_PRJ.jpg", "a_DE_PRJ.jpg", "a_DE_PRJ.jpg", "a_DE_PRJ.jpg"),
    T(12, List(
      "a_DE_PRJ.jpg",
      "a_EN_PRJ.jpg"),
      "a_DE_PRJ.jpg", "a_EN_PRJ.jpg", "a_DE_PRJ.jpg", "a_EN_PRJ.jpg"),
    T(13, List(
      "a_PRJ.jpg",
      "a_START.jpg"),
      "a_START.jpg", "a_START.jpg", "a_PRJ.jpg", "a_PRJ.jpg"))

  testvalues.foreach { t =>
    test("%3d ImageLangLoc %s" format (t.nr, t.files.mkString(" "))) {
      val il = ImageLangLocFactory.toImageLangLoc(t.files.map { x => II(x, 0, 0) })
      assert(il.imageInfo(Eng, ILStart).name === t.engStart)
      assert(il.imageInfo(Eng, ILProject).name === t.engPrj)
      assert(il.imageInfo(Ger, ILStart).name === t.gerStart)
      assert(il.imageInfo(Ger, ILProject).name === t.gerPrj)
    }

  }

}
