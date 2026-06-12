package net.entelijan.kimi.imgtransform

import os.Path
object ImageTransformFilesKimi extends App {

  // Define source and destination paths using the home directory
  val home          = os.home
  val srcDir: Path  = home / "tmp" / "kimi" / "images"
  val destDir: Path = home / "tmp" / "kimi" / "images-out"

  // Ensure output directory exists
  os.makeDir.all(srcDir)
  os.makeDir.all(destDir)

  if (!os.exists(srcDir)) {
    println(s"Error: Source directory '$srcDir' does not exist.")
    sys.exit(1)
  }

  // Set of valid image extensions to filter out non-image files
  val validExtensions: Set[String] = Set("jpg", "jpeg", "png", "webp", "bmp", "tiff")

  var processedCount = 0

  println("Starting image scaling in Scala...")

  // List files in the source directory
  os.list(srcDir).foreach { filePath =>
    val ext = filePath.ext.toLowerCase

    // Check if it's a file and has a valid image extension
    if (os.isFile(filePath) && validExtensions.contains(ext)) {
      val output = destDir / filePath.last

      println(s"Processing: ${filePath.last}...")

      try {
        // Execute the ImageMagick command line tool
        // "500x500>" scales the longer side to 500px, preserves aspect ratio,
        // and ignores images already smaller than 500px.
        os.proc("magick", filePath.toString, "-resize", "500x500>", output.toString).call()
        processedCount += 1
      } catch {
        case e: os.SubprocessException =>
          println(s"Failed to process ${filePath.last}. ImageMagick error: ${e.result.toString}")
        case _: java.io.IOException =>
          println(
            "Error: 'magick' command not found. Ensure ImageMagick is installed and in your PATH."
          )
          sys.exit(1)
      }
    }
  }

  println(s"\nTask complete! Successfully processed $processedCount images.")
  println(s"Output saved to: $destDir")

}
