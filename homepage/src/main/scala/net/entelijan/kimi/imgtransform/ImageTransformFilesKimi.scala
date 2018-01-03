package net.entelijan.kimi.imgtransform

import java.io.File

object ImageTransformFilesKimi extends App {

  val indir = new File("/Users/wwagner4/prj/hp-kimi/input/bilder")
  val outdir = new File("/Users/wwagner4/prj/hp-kimi/homepage/src/main/web/images")
  val maxWidth = 500
  val maxHeight = 500

  ImageTransformFiles.runInOutDir(indir, outdir, maxWidth, maxHeight)

}