package net.entelijan.kimi

object BuildKimiHomepage extends App {
  KimiUtil.clearDirectoryRecursive(KimiUtil.outputDir)
  println (s"cleared ${KimiUtil.outputDir}")
  Homepage.build()
}
