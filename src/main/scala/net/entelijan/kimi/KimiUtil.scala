package net.entelijan.kimi

import java.io.File

object KimiUtil {

  def uqual(in: String): String = {
    val R = """(.*)\(.*\)""".r
    in match {
      case R(a) => a.trim()
      case _    => in
    }
  }

  def inputFile: File = new File(FileUtil.inputDir, "Projekte.csv")

  def outputDir: File = FileUtil.outputDir

  private object FileUtil {

    def homeDir: File = getOrCreateDir(None, System.getProperty("user.home"))
    def generalWorkDir: File = getOrCreateDir(Some(homeDir), "work")
    def myWorkDir: File = getOrCreateDir(Some(generalWorkDir), "work-html-hp-k")
    def outputDir: File = getOrCreateDir(Some(myWorkDir), "output")
    def inputDir: File = getOrCreateDir(Some(myWorkDir), "input")

    def getOrCreateDir(parent: Option[File], path: String): File = {
      val re = parent.map(p => new File(p, path)).getOrElse(new File(path))
      if (!re.exists()) {
        re.mkdirs()
        re
      } else if (re.isDirectory) {
        re
      } else {
        throw new IllegalStateException(s"$parent - ${path} exists but is not a directory.")
      }
    }

  }

}