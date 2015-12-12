package com.stderr.mandelbulb

import java.awt.Color

import com.google.common.base.Preconditions.checkArgument

case class Canvas(val width: Integer, val height: Integer) {
  val rasterLines = Map[Int, RasterLine]().withDefaultValue(RasterLine(width))

  def setPixel(point: Point, pixel: Pixel) = {
    checkArgument(point.x >= 0 && point.x < width, "X parameter out of range", point.x)
    checkArgument(point.y >= 0 && point.y < height, "Y parameter out of range", point.y)
    val rasterLine = rasterLines(point.y)
    rasterLine.setPixel(pixel, point.x)
  }

  def getRGB(point: Point): Int = {
    checkArgument(point.y >= 0 && point.y < height, "Y parameter out of range", point.y)
    val rasterLine = rasterLines(point.y)
    val pixel = rasterLine.getPixel(point.x)
    val color = new Color(pixel.red.toInt, pixel.green.toInt, pixel.blue.toInt)
    color.getRGB
  }
}

case class RasterLine(val width: Integer) {
  val pixels = Array.fill[Pixel](width)(Pixel(0, 0, 0))

  def setPixel(pixel: Pixel, x: Integer) = {
    checkArgument(x >= 0 && x < width, "X parameter out of range", x)
    pixels(x) = pixel
  }

  def getPixel(x: Integer) = {
    checkArgument(x >= 0 && x < width, "X parameter out of range", x)
    pixels(x)
  }

}

object Point {
  def apply(pair: (Int, Int)): Point = new Point(pair._1, pair._2)
}

case class Point(val x: Integer, val y: Integer)

object Pixel {
  def toRGB(pixel: Pixel): Int = {
    ((pixel.red & 0xFF << 16) | (pixel.green & 0xFF << 8) | (pixel.blue & 0xFF)).toInt
  }
}

case class Pixel(val red: Long, val green: Long, val blue: Long)
