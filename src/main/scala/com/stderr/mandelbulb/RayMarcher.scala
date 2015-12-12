package com.stderr.mandelbulb

import scala.collection.immutable.IndexedSeq

case class Scene (imageWidth: Int, imageHeight: Int) {
  val DEPTH_OF_FIELD = 2.5
  var eyeDistanceFromNearField = 2.2

  val cHalfWidth = imageWidth.toDouble / 2.0
  val pixel = DEPTH_OF_FIELD / ((imageHeight.toDouble + imageWidth.toDouble) / 2.0)
  val halfPixel = pixel / 2.0

  lazy val points = for (x <- 0 to imageWidth - 1;
                         y <- 0 to imageHeight -1) yield Some(Point(x, y))
}

case class MarchedRay(scene: Scene, point: Point,
                      rayLocation: Vec3, rayDirection: Vec3,
                      distanceFromCamera: Double, iterations: Int) {
}

case class RayMarcher(scene: Scene) {

  val MAX_ITER = 5000

  var viewDirection = Vec3()
  var lightDirection = Vec3()
  var eyeLocation = Vec3()
  var nearFieldLocation = Vec3()

  def computeScene(DE: Vec3 => Double) = {
    for (y <- 0 to scene.imageHeight - 1;
         rasterLine <- computeScanLine(y, DE)) yield rasterLine
  }

  def computeScanLine(y: Int, DE: Vec3 => Double) = {
    for (x <- 0 to scene.imageWidth - 1;
         ray <- computeRay(Point(x, y), DE);
         pixel <- computeColor(ray, DE)) yield (Some(pixel), Some(ray))
  }

  def computeRay(point: Point, DE: Vec3 => Double) = {
    val x = point.x
    val y = point.y
    val ny = y - scene.imageHeight / 2;

    // scalarMultiply(crossProduct(turnOrthogonal(setTo(tempViewDirectionY, viewDirection)), viewDirection), ny*pixel);
    val tempViewDirectionY = viewDirection.turnOrthogonal.crossProduct(viewDirection) * (ny * scene.pixel)
    // turnOrthogonal(setTo(tempViewDirectionX1, viewDirection));
    val tempViewDirectionX1 = viewDirection.turnOrthogonal

    val nx = x - scene.cHalfWidth

    // setTo(pixelLocation, nearFieldLocation);
    var pixelLocation: Vec3 = nearFieldLocation

    //scalarMultiply(setTo(tempViewDirectionX2, tempViewDirectionX1), nx*pixel);
    val tempViewDirectionX2 = tempViewDirectionX1 * (nx * scene.pixel)

    // add(pixelLocation, tempViewDirectionX2);
    // add(pixelLocation, tempViewDirectionY);
    pixelLocation = pixelLocation + tempViewDirectionX2
    pixelLocation = pixelLocation + tempViewDirectionY

    // setTo(rayLocation, pixelLocation);
    var rayLocation = pixelLocation

    // normalize(subtract(setTo(rayDirection, rayLocation), eyeLocation));
    val rayDirection = (rayLocation - eyeLocation).normalize

    var d = DE(rayLocation);
    var iterations = 0
    var distanceFromCamera = 0.0
    var done = false
    while (!done) {
      if (iterations >= MAX_ITER || d < scene.halfPixel) {
        done = true
      } else {
        ////Increase rayLocation with direction and d:
        //add(rayLocation, scalarMultiply(rayDirection, d));
        rayLocation = rayLocation + (rayDirection * d)
        //And reset ray direction:
        // rayDirection = rayDirection.normalize

        //Move the pixel location:
        //distanceFromCamera = length(subtract(setTo(temp, nearFieldLocation), rayLocation));
        distanceFromCamera = (nearFieldLocation - rayLocation).length
        if (distanceFromCamera > scene.DEPTH_OF_FIELD) {
          done = true
        } else {
          d = DE(rayLocation)
        }
        iterations += 1
      }
    }

    //      println(s"($x,$y) distanceFromCamera $distanceFromCamera iterations $iterations")

    Some(MarchedRay(scene, point, rayLocation, rayDirection,
      distanceFromCamera, iterations))
  }

  def computeColor(ray: MarchedRay, DE: Vec3 => Double) = {
    val rayLocation = ray.rayLocation.clone
    val rayDirection = ray.rayDirection.clone
    if (ray.distanceFromCamera > 0 && ray.distanceFromCamera < ray.scene.DEPTH_OF_FIELD) {
      val smallStep = 0.01
      val bigStep = 0.02

      rayLocation.x -= smallStep
      val locationMinX = DE(rayLocation)
      rayLocation.x += bigStep
      val locationPlusX = DE(rayLocation)
      rayLocation.x -= smallStep

      rayLocation.y -= smallStep
      val locationMinY = DE(rayLocation)
      rayLocation.y += bigStep;
      val locationPlusY = DE(rayLocation)
      rayLocation.y -= smallStep

      rayLocation.z -= smallStep
      val locationMinZ = DE(rayLocation)
      rayLocation.z += bigStep
      val locationPlusZ = DE(rayLocation)
      rayLocation.z -= smallStep

      //Calculate the normal:
      //        normal[0] = (locationMinX - locationPlusX);
      //        normal[1] = (locationMinY - locationPlusY);
      //        normal[2] = (locationMinZ - locationPlusZ);
      //        normalize(normal);
      val normal = Vec3((locationMinX - locationPlusX),
        (locationMinY - locationPlusY),
        (locationMinZ - locationPlusZ)).normalize

      val dotNL = lightDirection.dot(normal)
      val diff = Vec3.clampRange(dotNL, 0.0, 1.0)

      //Calculate specular light:
      //normalize(add(setTo(halfway, rayDirection), lightDirection));

      val halfway = (rayDirection + lightDirection).normalize

      // var dotNH = dotProduct(halfway, normal);
      var dotNH = halfway * normal
      // var spec = Math.pow(saturate(dotNH),35);
      val spec = Math.pow(dotNH.saturate.x, 35)

      val shad = shadow(1.0, ray.scene.DEPTH_OF_FIELD, 16.0, rayLocation, DE) + 0.25

      // TODO diff.x ? vector
      val brightness = (10.0 + (200.0 + spec * 45.0) * shad * diff) / 270.0

      var red = 10 + (380 * brightness)
      var green = 10 + (280 * brightness)
      var blue = (180 * brightness)

      red = Vec3.clampRange(red, 0, 255.0)
      green = Vec3.clampRange(green, 0, 255.0)
      blue = Vec3.clampRange(blue, 0, 255.0)

      Some(Pixel(red.round, green.round, blue.round))

    } else {
      val red = 155 + Vec3.clampRange(ray.iterations * 1.5, 0.0, 100.0)
      val green = 205 + Vec3.clampRange(ray.iterations * 1.5, 0.0, 50.0)
      Some(Pixel(red.round, green.round, 255))
    }
  }

  private def shadow(mint: Double, maxt: Double, k: Double, rayLocation: Vec3, DE: Vec3 => Double): Double = {
    var res = 1.0
    var t = mint
    while (t < maxt) {
      val rd = lightDirection * t
      val ro = rayLocation - rd
      var h = DE(ro);
      if (h < 0.001) {
        return 0.0;
      }
      res = Math.min(res, k * h / t)
      t += h
    }
    res
  }

  private def toRad(angle: Double) = {
    angle * Math.PI / 180.0
  }

  // Construction calculations
  val lightAngle = 140.0
  val viewAngle = 150.0
  val rad = toRad(lightAngle)
  val lightX = Math.cos(rad) * scene.DEPTH_OF_FIELD / 2.0
  val lightZ = Math.sin(rad) * scene.DEPTH_OF_FIELD / 2.0

  val lightLocation: Vec3 = Vec3(lightX, (scene.DEPTH_OF_FIELD / 2), lightZ)
  //normalize(subtract(setTo(lightDirection, NUL), lightLocation));
  lightDirection = (lightDirection - lightLocation).normalize


  val viewRad = toRad(viewAngle)
  val viewX = Math.cos(viewRad) * scene.DEPTH_OF_FIELD / 2.0
  val viewZ = Math.sin(viewRad) * scene.DEPTH_OF_FIELD / 2.0
  nearFieldLocation = Vec3(viewX, 0.0, viewZ)

  // normalize(subtract(setTo(viewDirection, NUL), nearFieldLocation));
  viewDirection = (Vec3() - nearFieldLocation).normalize
  //    scalarMultiply(setTo(reverseDirection, viewDirection), eyeDistanceFromNearField);
  val reverseDirection: Vec3 = viewDirection * scene.eyeDistanceFromNearField
  //subtract(setTo(eyeLocation, nearFieldLocation), reverseDirection);
  eyeLocation = nearFieldLocation - reverseDirection
}
