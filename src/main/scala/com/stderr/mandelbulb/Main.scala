package com.stderr.mandelbulb

import java.awt.Color
import java.io.{BufferedOutputStream, FileOutputStream}

import org.apache.log4j.{Level, Logger, BasicConfigurator}
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import org.apache.spark.rdd.RDD

import scala.collection.immutable.IndexedSeq

/*
* Ray marching MandelBulb in Scala
*/
object Main {
  val logger = Logger.getLogger(Main.getClass)

  def main(args: Array[String]):Unit = {
    // Simple scene generation
    // val rayMarcher = RayMarcher(scene)
    // the pixels of the image as (pixel, ray)
    // val marchedRays = rayMarcher.computeScene(mandelbulb)
    val imageWidth = 400
    val imageHeight = 400
    val marchedRays = sparkParallelComputeScene(imageWidth, imageHeight, mandelbulb)
    writePPM(imageWidth, imageHeight, marchedRays)
  }

  def sparkParallelComputeScene(imageWidth: Int, imageHeight: Int, DE: Vec3 => Double): Seq[(Option[Pixel], Option[MarchedRay])] = {
    BasicConfigurator.configure()
    Logger.getRootLogger.setLevel(Level.ERROR)

    val conf = new SparkConf().setAppName("mandelbulb").setMaster("local")
    val sc = new SparkContext(conf)

    val scenes = sc.parallelize(1 to 1).map(Scene(imageWidth, imageHeight, _))
    val xDimension = sc.parallelize(0 to imageWidth - 1, 8)
    val yDimension = sc.parallelize(0 to imageHeight - 1, 8)
    val xy = xDimension.cartesian(yDimension).map(Point(_))

    val rays = xy.cartesian(scenes).map((p: (Point, Scene)) => makeRay(p._2, p._1, DE))
    val pixels = rays.map((m: Option[MarchedRay]) => makePixel(m, DE))
    pixels.sortBy(sortKey).collect().toSeq
  }

  def makeRay(scene: Scene, point: Point, DE: Vec3 => Double): Option[MarchedRay]  = {
    val marcher = new RayMarcher(scene)
    marcher.computeRay(point, DE)
  }

  def makePixel(marchedRay: Option[MarchedRay], DE: Vec3 => Double):(Option[Pixel],Option[MarchedRay]) = {
    if (marchedRay.isDefined) (ColorComputer.computeColor(new RayMarcher(marchedRay.get.scene).lightDirection, marchedRay.get, DE), marchedRay) else (Some(Pixel(0, 0, 0)), marchedRay)
  }

  def sortKey(pair: (Option[Pixel], Option[MarchedRay])): Tuple3[Int, Int, Int] = {
    pair match {
      case (_, Some(ray)) => (ray.scene.frame, ray.point.x, ray.point.y)
      case _ => (Int.MaxValue, Int.MaxValue, Int.MaxValue)
    }
  }

  def writePPM(imageWidth: Int, imageHeight: Int, marchedRays: Seq[(Option[Pixel], Option[MarchedRay])]) = {
    val out = new BufferedOutputStream(new FileOutputStream("t.ppm"))
    val header = s"P3\n$imageWidth $imageHeight\n255\n"
    out.write(header.getBytes)
    for (marchedRay <- marchedRays;
         pixel <- marchedRay._1;
         ray <- marchedRay._2;
         ppm <- Some(s"${pixel.red} ${pixel.green} ${pixel.blue}\n")) out.write(ppm.getBytes)
    out.close()
  }

  def writePNG(scene:Scene, marchedRays: Seq[(Option[Pixel], Option[MarchedRay])]) = {
    val bi: BufferedImage = new BufferedImage(scene.imageWidth, scene.imageHeight, BufferedImage.TYPE_INT_RGB)
    for (marchedRay <- marchedRays;
         pixel <- marchedRay._1;
         ray <- marchedRay._2;
         rgb <- Some(new Color(pixel.red.toInt, pixel.green.toInt, pixel.blue.toInt)))
      yield bi.setRGB(ray.point.x, ray.point.y, rgb.getRGB())
    ImageIO.write(bi, "png", new java.io.File("t.png"))
  }

  def sphere(v : Vec3): Double = {
    v.length - 1.0
  }

  def writeImageFile(canvas: Canvas) = {
    val bi: BufferedImage = new BufferedImage(canvas.width, canvas.height, BufferedImage.TYPE_INT_RGB)
    for (x <- 0 to canvas.width - 1) {
      for (y <- 0 to canvas.height - 1) {
        val rgb: Int = canvas.getRGB(Point(x, y))
        bi.setRGB(x, y, rgb)
      }
    }
    ImageIO.write(bi, "png", new java.io.File("t.png"))
  }

  def mandelbulb(pos: Vec3): Double = {
    val Iterations = 20.0
    val Power = 8
    var z = Vec3(pos.x, pos.y, pos.z)
    var dr = 1.0
    var r = 0.0
    var i = 0
    while (i < Iterations) {
      r = z.length
      if (r > 2.5) {  // DEPTH_OF_FIELD
        return 0.5 * Math.log(r) * r / dr
      }
      var theta = Math.acos(z.z / r)
      var phi = Math.atan2(z.y, z.x)
      dr =  Math.pow(r, Power - 1.0) * Power * dr + 1.0
      val zr = Math.pow(r, Power)
      theta = theta * Power
      phi = phi * Power
      val sinTheta = Math.sin(theta)
      z = Vec3(sinTheta * Math.cos(phi),
        Math.sin(phi) * sinTheta,
        Math.cos(theta))
      z = (z * zr) + pos
      i += 1
    }
    return 0.5 * Math.log(r) * r / dr
  }

}

