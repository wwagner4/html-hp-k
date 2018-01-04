package net.entelijan.kimi.imgtransform

import java.io.{File, PrintWriter}

object ImageTransformFiles {

  def runInOutDir(indir: File, outdir: File, maxWidth: Int, maxHeight: Int): Unit = {

    def runFile(file: File, h: Handler): Unit = {
      if (!file.isDirectory()) h.handle(file.getPath)
    }

    def runDir(indir: File, h: Handler): Unit = {
      if (!indir.isDirectory()) throw new IllegalArgumentException("Input directory %s is not a directory" format indir)
      val files = indir.listFiles().toList
      files.foreach { f => runFile(f, h) }
    }

    val scriptDirName = "/Users/wwagner4/Library/Application Support/GIMP/2.8/scripts"

    outdir.mkdirs()
    val h: Handler = new DefaultHandler(outdir.getAbsolutePath)
    Utils.deployScript(scriptDirName, maxWidth, maxHeight)
    println("Deployed script to " + scriptDirName)
    runDir(indir, h)
  }

}

trait Handler {

  def handle(filename: String): Unit

}

trait ToOtherFileHandler extends Handler {

  def convertToOutfile: (String) => String

  def handle(filename: String): Unit = {
    handleToOutFile(filename, convertToOutfile(filename))
  }
  def handleToOutFile(inFile: String, outFile: String): Unit

}

class DefaultHandler(outDirName: String) extends ToOtherFileHandler {
  import sys.process._

  def convertToOutfile = Utils.toSeparateDirectory(_)(outDirName)

  def handleToOutFile(in: String, out: String): Unit = {
    if (in.endsWith("jpg")) {
      println("Handling filename '%s' ---> '%s'" format (in, out))
      Utils.deleteFile(out)
      val contents = s"""/opt/local/bin/gimp -i -d -f -b '(kimi-conv "$in" "$out")' -b '(gimp-quit 0)'"""
      val tdir = System.getProperty("java.io.tmpdir")
      println("tmpdir %s" format tdir)
      val bashfile = "%s/%s" format (tdir, "kimi.sh")
      Utils.writeToFile(contents, bashfile)
      val re = s"bash $bashfile".!!
      println("----- finished execution ------")
      println(re)
    }
  }
}

object Utils {

  val pathSepaRegex = "/"
  val pathSepaOut = "/"

  def toSeparateDirectory(path: String)(outdir: String): String = {

    def toPngFile(fname: String): String = {
      val split = fname.split("\\.").toList.reverse
      def errmsg = "Filename '%s' contains no qualifier" format fname
      split match {
        case Nil       => throw new IllegalArgumentException(errmsg)
        case _ :: Nil  => throw new IllegalArgumentException(errmsg)
        case _ :: rest => rest.reverse.mkString(".") + ".jpg"
      }
    }

    val pathElems: List[String] = path.split(pathSepaRegex).toList
    val fileName = pathElems.reverse(0)
    outdir + pathSepaOut + toPngFile(fileName)

  }

  def script(maxWidth: Int, maxHeight: Int) = s"""
(define (info msg)
  (gimp-message (string-append "[INFO] " msg))
)

(define (kimi-conv in out)
  (gimp-message-set-handler 1)
  (info (string-append "converting " in " -> " out))
  (let* ((max-width $maxWidth)
         (max-height $maxHeight)
         (black (list 0 0 0))
         (image    (car (gimp-file-load RUN-NONINTERACTIVE in "")))
         (drawable (car (gimp-image-active-drawable image)))
         (cur-width  (car (gimp-image-width image)))
         (cur-height (car (gimp-image-height image)))
         (scale (if (< cur-width cur-height)     
           (if (< cur-height max-height) 1 (/ max-height cur-height))
           (if (< cur-width max-width) 1 (/ max-width cur-width)))
         )
         (new-width  (* cur-width scale))
         (new-height (* cur-height scale))
        )
     (info (string-append "scale: " (number->string scale)))   
     (info (string-append "width: " (number->string new-width)))   
     (info (string-append "height: " (number->string new-height)))   
     
     (gimp-image-scale-full image new-width new-height 2)
     ;(script-fu-drop-shadow image drawable 6 6 25 black 35 1)
     ;(set! drawable (car (gimp-image-merge-visible-layers image 0)))     
     ;(file-png-save2 RUN-NONINTERACTIVE image drawable out out 1 3 1 1 1 1 1 1 1)
     (file-jpeg-save 1 image drawable out out 
              0.9 0 1 1 "Kimi-Homepage" 0 0 0 0 )
     (info (string-append "converted " in " -> " out))
  )
)
    """

  def deployScript(scriptDir: String, maxWidth: Int, maxHeight: Int): Unit = {
    val fnam: String = scriptDir + pathSepaOut + "kimi-convert.scm"
    writeToFile(script(maxWidth, maxHeight), fnam)
  }

  def writeToFile(content: String, fname: String): Unit = {
    val file = new File(fname)
    val pw = new PrintWriter(file)
    pw.print(content)
    pw.close()
  }

  def deleteFile(fname: String): Unit = {
    val f = new File(fname)
    if (f.exists()) f.delete()
  }
}

