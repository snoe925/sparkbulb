package com.stderr.mandelbulb

import scala.collection.immutable.IndexedSeq

case class Scene (imageWidth: Int, imageHeight: Int, frame: Int = 0) {
  val DEPTH_OF_FIELD = 2.5
  val eyeDistanceFromNearField = 2.2

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

  def computeScene(DE: Vec3 => Double) = {
    for (y <- 0 to scene.imageHeight - 1;
         rasterLine <- computeScanLine(y, DE)) yield rasterLine
  }

  def computeScanLine(y: Int, DE: Vec3 => Double) = {
    for (x <- 0 to scene.imageWidth - 1;
         ray <- computeRay(Point(x, y), DE);
         pixel <- ColorComputer.computeColor(lightDirection, ray, DE)) yield (Some(pixel), Some(ray))
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
    //scalarMultiply(setTo(tempViewDirectionX2, tempViewDirectionX1), nx*pixel);
    val tempViewDirectionX2 = tempViewDirectionX1 * (nx * scene.pixel)

    // add(pixelLocation, tempViewDirectionX2);
    // add(pixelLocation, tempViewDirectionY);
    val pixelLocation = nearFieldLocation + tempViewDirectionX2 + tempViewDirectionY

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
  val lightDirection = (Vec3() - lightLocation).normalize


  val viewRad = toRad(viewAngle)
  val viewX = Math.cos(viewRad) * scene.DEPTH_OF_FIELD / 2.0
  val viewZ = Math.sin(viewRad) * scene.DEPTH_OF_FIELD / 2.0
  val nearFieldLocation = Vec3(viewX, 0.0, viewZ)

  // normalize(subtract(setTo(viewDirection, NUL), nearFieldLocation));
  val viewDirection = (Vec3() - nearFieldLocation).normalize
  //    scalarMultiply(setTo(reverseDirection, viewDirection), eyeDistanceFromNearField);
  val reverseDirection: Vec3 = viewDirection * scene.eyeDistanceFromNearField
  //subtract(setTo(eyeLocation, nearFieldLocation), reverseDirection);
  val eyeLocation = nearFieldLocation - reverseDirection
}
