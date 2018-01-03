package net.entelijan.kimi.imgtransform

import java.io.File

object ImageTransformFilesTaschenfahrradFlat extends App {

  val indir = new File("/Users/wwagner4/prj/taschenfahrrad/taschenfahrrad/work/fotos2016_01")
  val outdir = new File("/Users/wwagner4/prj/taschenfahrrad/taschenfahrrad/work/fotos2016transformed1")
  outdir.mkdirs()

  val maxWidth = 900
  val maxHeight = 600

  ImageTransformFiles.runInOutDir(indir, outdir, maxWidth, maxHeight)

}