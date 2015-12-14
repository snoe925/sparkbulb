package com.stderr.mandelbulb

/**
  * Compute the pixel color value for ray
  */
object ColorComputer {

  def computeColor(lightDirection: Vec3, ray: MarchedRay, DE: Vec3 => Double) = {
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

      val shad = shadow(lightDirection, 1.0, ray.scene.DEPTH_OF_FIELD, 16.0, rayLocation, DE) + 0.25

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

  private def shadow(lightDirection: Vec3, mint: Double, maxt: Double, k: Double, rayLocation: Vec3, DE: Vec3 => Double): Double = {
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

}
