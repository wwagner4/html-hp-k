package net.entelijan.kimi

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileSystems, Files, Path}

object ResCopy {

  def copy(from: File, to: File, ignore: List[String]): Unit = {
    def accept(f: File, i: List[String]): Boolean = i match {
      case Nil => true
      case m :: rest => if(f.getName.matches(m)) false else accept(f, rest)
    }
    require(from.isDirectory, "%s is not a directory" format from)
    require(to.isDirectory, "%s is not a directory" format to)
    val toFiles = to.listFiles().toList
    for (fromFile <- from.listFiles()) {
      if (accept(fromFile, ignore)) {
        if (fromFile.isDirectory) {
          findFile(fromFile.getName, toFiles) match {
            case None =>
              val newDir = new File(to, fromFile.getName)
              newDir.mkdirs()
              copy(fromFile, newDir, ignore)
            case Some(toFile) => copy(fromFile, toFile, ignore)
          }
        } else {
          findFile(fromFile.getName, toFiles) match {
            case None => copyFile(fromFile, to)
            case Some(toFile) => if (leftIsYounger(fromFile, toFile)) copyFile(fromFile, to)
            //else println("no copy of %s to %s because younger exists" format (fromFile, to))
          }
        }
      } else {
        println("ignored %s" format fromFile)
      }
    }

  }

  private def copyFile(f: File, dir: File): Unit = {
    import java.io.{File, FileInputStream, FileOutputStream}

    import scala.language.postfixOps
    require(dir.isDirectory, "%s is not a directory" format dir)
    val newFile = new File(dir, f.getName)
    new FileOutputStream(newFile) getChannel () transferFrom (
      new FileInputStream(f) getChannel, 0, Long.MaxValue)
    println("copied %s to %s" format (f, dir))
  }

  private def leftIsYounger(left: File, right: File): Boolean = {
    def time(f: File): Long = {
      val p: Path = FileSystems.getDefault.getPath(f.getAbsolutePath)
      val attr = Files.readAttributes(p, classOf[BasicFileAttributes])
      attr.lastModifiedTime().toMillis
    }
    val re = time(left) > time(right)
    //if (true) println("*** left is younger l: %s r: %s" format (left.getPath(), right.getPath()))
    re
  }

  private def findFile(name: String, files: List[File]): Option[File] = {
    files match {
      case Nil => None
      case f :: rest => if (f.getName == name) Some(f)
      else findFile(name, rest)
    }
  }

}
