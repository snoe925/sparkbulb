package com.stderr.mandelbulb

import java.io.{BufferedOutputStream, FileOutputStream}

import org.apache.log4j.{Level, Logger, BasicConfigurator}
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD

/*
* Ray marching MandelBulb in Scala
*/
object Main {
  val logger = Logger.getLogger(Main.getClass)

  def main(args: Array[String]):Unit = {
    val imageWidth = 400
    val imageHeight = 400
    val master = if (args.length > 0) args(0) else "local"
    sparkParallelComputeScene(imageWidth, imageHeight, mandelbulb, master)
  }

  def sparkParallelComputeScene(imageWidth: Int, imageHeight: Int, DE: Vec3 => Double, master: String) = {
    BasicConfigurator.configure()
    Logger.getRootLogger.setLevel(Level.ERROR)

    val conf = new SparkConf().setAppName("mandelbulb").setMaster(master)
    val sc = new SparkContext(conf)

    // RDD[Scene]
    val scenes = sc.parallelize(0 to 1).map(Scene(imageWidth, imageHeight, _))
    val xDimension = sc.parallelize(0 to imageWidth - 1, 8)
    val yDimension = sc.parallelize(0 to imageHeight - 1, 8)
    // RDD[Point]
    val xy = xDimension.cartesian(yDimension).map(_ match { case (x,y) => Point(x, y) })
    // RDD[Option[MarchedRay]]
    val rays: RDD[(Scene, Point, Option[MarchedRay])] = scenes.cartesian(xy).map((p: (Scene, Point)) => {
      val ray = makeRay(p._1, p._2, DE)
      (p._1, p._2, ray)
    })
    // RDD[(Option[Pixel], Option[MarchedRay])]
    def mp(p:(Scene, Point, Option[MarchedRay])) = makePixel(p._1, p._2, p._3, DE)
    val pixels: RDD[(Scene, Point, Option[Pixel], Option[MarchedRay])] = rays.map(mp)
    // RDD[(frame: Int, (point: Point, pixel: Pixel))]
    val indexByFrame: RDD[(Int, (Scene, Point, Pixel))] = pixels.map(_ match {
      case (scene, point, Some(pixel), Some(ray)) => (scene.frame, (scene, point, pixel))
      case (scene, point, _, _) => (scene.frame, (scene, point, Pixel(255, 0, 0)))
    })
    // RDD[(Int, Iterable[(Scene, Point, Pixel)])]
    val pixelsGroupedByFrame = indexByFrame.groupByKey()
    // RDD[(Int, Seq[(Scene, Point, Pixel)])]
    val sortedPixels = pixelsGroupedByFrame.mapValues(_.toSeq.sorted)
    sortedPixels.mapValues(writeFrame).collect()
  }

  def makeRay(scene: Scene, point: Point, DE: Vec3 => Double): Option[MarchedRay]  = {
    val marcher = new RayMarcher(scene)
    marcher.computeRay(point, DE)
  }

  def makePixel(scene: Scene, point: Point, marchedRay: Option[MarchedRay], DE: Vec3 => Double):(Scene, Point, Option[Pixel],Option[MarchedRay]) = {
    if (marchedRay.isDefined)
      (scene, point, ColorComputer.computeColor(scene.lightDirection, marchedRay.get, DE), marchedRay)
    else (scene, point, Some(Pixel(0, 0, 0)), marchedRay)
  }

  def sortKeyX(pair: (Option[Pixel], Option[MarchedRay])): Tuple3[Int, Int, Int] = {
    pair match {
      case (_, Some(ray)) => (ray.scene.frame, ray.point.x, ray.point.y)
      case _ => (Int.MaxValue, Int.MaxValue, Int.MaxValue)
    }
  }

  def writeFrame(pixels: Seq[(Scene, Point, Pixel)]): Boolean = {
    if (pixels.isEmpty) false else {
      val (scene: Scene, _, _) = pixels.head
      val fileName = "frames/t%06d.ppm".format(scene.frame)
      val out = new BufferedOutputStream(new FileOutputStream(fileName))
      val header = s"P3\n${scene.imageWidth} ${scene.imageHeight}\n255\n"
      out.write(header.getBytes)
      for ((scene, point, pixel) <- pixels) out.write(s"${pixel.red} ${pixel.green} ${pixel.blue}\n".getBytes())
      out.close()
      true
    }
  }

  def sphere(v : Vec3): Double = {
    v.length - 1.0
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

