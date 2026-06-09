package net.entelijan.kimi

import org.apache.poi.ss.usermodel._

import java.util
import java.io.{File, FileInputStream}

object Tryout extends App {
  println("Tryout")

  val filePath = "/home/wwagner4/tmp/ProjekteEinfach.xlsx"

  val file     = new File(filePath)
  val workbook = WorkbookFactory.create(file)

  val sheet = workbook.getSheet("main")

  println(s"sheet: $sheet")

  for (i <- 0 to 200) {
    val row = sheet.getRow(i)
    if (row != null) {
      for (j <- 0 to 100) {
        val cell = row.getCell(j)
        if (cell != null) {
          val ct = cell.getCellType()
          val v = ct match {
            case CellType.STRING => cell.getStringCellValue()
            case _               => ""
          }
          println(f"cell $i $j $v")
        }
      }
      println("---------------------------------------------")
    }
  }

}
