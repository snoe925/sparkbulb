package com.stderr.mandelbulb

import java.util.concurrent.atomic.AtomicInteger

import org.apache.commons.io.FileUtils
import org.jcodec.common.model.{ColorSpace, Picture}
import org.jcodec.movtool.Paste
import org.jcodec.api.SequenceEncoder

/**
  * Convert the collection of (Point, Pixel) into MP4
  */
object PixmapToMP4 {

  val fileCounter = new AtomicInteger(0)

  def combine(fileNameOption: Option[String], pixmap: Iterable[(Scene, Point, Pixel)]): Option[String] = {
    val fileName = if (fileNameOption.isDefined) fileNameOption.get else tempFileName
    val javaFile: java.io.File = new java.io.File(fileName)
    val scene = pixmap.head._1
    // A red, green and blue color plane
    val data:Array[Array[Int]] = new Array[Array[Int]](1)
    data(0) = new Array[Int](scene.imageHeight * scene.imageWidth * 3)
    for ((_, point, pixel) <- pixmap) {
      val offset = 3 * (point.x + point.y * scene.imageWidth);
      // Use offset colors
      data(0)(offset) = pixel.red.toInt
      data(0)(offset + 1) = pixel.green.toInt
      data(0)(offset + 2) = pixel.blue.toInt
    }

    val pic: Picture = new Picture(scene.imageWidth, scene.imageHeight, data, ColorSpace.RGB)
    val encoder = new SequenceEncoder(javaFile)
    encoder.encodeNativeFrame(pic)
    encoder.finish()
    Some(fileName)
  }

  def merge(firstFile: Option[String], secondFile: Option[String]): Option[String] = {
    val tempFile = tempFileName
    val f1: java.io.File = new java.io.File(firstFile.get)
    val f2: java.io.File = new java.io.File(tempFile)
    FileUtils.copyFile(f1, f2)
    val args = Array[String](f2.getPath, secondFile.get)
    Paste.main(args)
    Some(tempFile)
  }

  def encode(bits: Array[(Int, Seq[(Scene, Point, Pixel)])], fileName: String): Unit = {
    val javaFile: java.io.File = new java.io.File(fileName)
    val scene = bits(0)._2.head._1
    // A red, green and blue color plane
    val data: Array[Array[Int]] = new Array[Array[Int]](1)
    data(0) = new Array[Int](scene.imageHeight * scene.imageWidth * 3)
    val encoder = new SequenceEncoder(javaFile)
    for ((frameNumber, pixmap) <- bits) {
      for ((_, point, pixel) <- pixmap) {
        val offset = 3 * (point.x + point.y * scene.imageWidth);
        // Use offset colors
        data(0)(offset) = pixel.red.toInt
        data(0)(offset + 1) = pixel.green.toInt
        data(0)(offset + 2) = pixel.blue.toInt
      }
      val pic: Picture = new Picture(scene.imageWidth, scene.imageHeight, data, ColorSpace.RGB)
      encoder.encodeNativeFrame(pic)
    }
    encoder.finish()
  }

  def tempFileName = "t%09d.mp4".format(fileCounter.incrementAndGet())
}

class PixmapToMP4 {
  val javaFile = new java.io.File("t.mp4")
  val encoder = new SequenceEncoder(javaFile)

  def encode(bits: Seq[(Scene, Point, Pixel)]) = {
    val scene = bits.head._1
    // A red, green and blue color plane
    val data: Array[Array[Int]] = new Array[Array[Int]](1)
    data(0) = new Array[Int](scene.imageHeight * scene.imageWidth * 3)
    for ((_, point, pixel) <- bits) {
      val offset = 3 * (point.x + point.y * scene.imageWidth);
      // Use offset colors
      data(0)(offset) = pixel.red.toInt
      data(0)(offset + 1) = pixel.green.toInt
      data(0)(offset + 2) = pixel.blue.toInt
    }
    val pic: Picture = new Picture(scene.imageWidth, scene.imageHeight, data, ColorSpace.RGB)
    encoder.encodeNativeFrame(pic)
  }

  def finish = encoder.finish()
}