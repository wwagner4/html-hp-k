package net.entelijan.kimi.imgtransform

import java.io.File

object ImageTransformFilesTaschenfahrrad extends App {

  val baseindir = new File("/Users/wwagner4/prj/taschenfahrrad/taschenfahrrad/generator/src/main/web/images")
  val baseoutdir = new File("/Users/wwagner4/prj/taschenfahrrad/taschenfahrrad/work/images")
  baseoutdir.mkdirs()
  
  val maxWidth = 900
  val maxHeight = 600

  baseindir.listFiles().foreach { indir => 
    if (indir.isDirectory) {
      val name = indir.getName
      val outdir = new File(baseoutdir, name)
      println("--- processing directory %s ---" format indir)
      ImageTransformFiles.runInOutDir(indir, outdir, maxWidth, maxHeight)
    }
  }
  

}