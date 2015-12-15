/*
 * From https://raw.githubusercontent.com/HairyFotr/AngryPigs/master/src/main/scala/AngryPigs/Geometry.scala
 * https://github.com/HairyFotr/AngryPigs
 */
package com.stderr.mandelbulb

import math._

object Vec3 {
  def apply(): Vec3 = new Vec3
  def apply(x: Double, y: Double, z: Double): Vec3 = new Vec3(x, y, z)
  def clampRange(n: Double, min: Double, max: Double) = Math.max(min, Math.min(n, max))
}

class Vec3(var x: Double, var y: Double, var z: Double) extends Serializable {
  def this() = this(0d, 0d, 0d)

  private def setPoints(v: Vec3) { x=v.x; y=v.y; z=v.z; }
  private def setPoints(x: Double, y: Double, z: Double) { this.x=x; this.y=y; this.z=z; }
  private def setPoints(p: Array[Double]): Unit = setPoints(p(0), p(1), p(2))

  override def clone: Vec3 = Vec3(x,y,z)
  private def each(f: Double => Double) { x = f(x); y = f(y); z = f(z); }
  private def map(f: Double => Double): Vec3 = { val out = this.clone; out.each(f); out }
  def applyVector(v: Vec3, multi: Double = 1): Unit = setPoints(this + (v * multi))

  def unary_- : Vec3      = Vec3(-x, -y, -z)
  def +(v: Vec3): Vec3    = Vec3(x+v.x, y+v.y, z+v.z)
  def -(v: Vec3): Vec3    = Vec3(x-v.x, y-v.y, z-v.z)
  def +=(v: Vec3): Unit   = setPoints(this + v)
  def +=(f: Double): Unit  = this.each(_ + f)
  def -=(v: Vec3): Unit   = setPoints(this + (-v))
  def -=(f: Double): Unit  = this.each(_ - f)
  def *(v: Vec3): Vec3    = Vec3(x*v.x, y*v.y, z*v.z)
  def *(f: Double): Vec3   = this.map(_ * f)
  def *=(f: Double): Unit  = this.each(_ * f)
  def /(f: Double): Vec3   = this.map(_ / f)
  def X(v: Vec3): Vec3    = Vec3(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)
  def dot(v: Vec3): Double = x*v.x + y*v.y + z*v.z

  //maybe this needs to be normalized too
  def angle(v: Vec3): Double = (180f/Pi * acos((this dot v)/v.length)).toDouble

  def length: Double = sqrt(this dot this)
  def ==(v: Vec3): Boolean = x == v.x && y == v.y && z == v.z
  def !=(v: Vec3): Boolean = !(this == v)

  def maxCoords(v: Vec3): Vec3 = Vec3(max(v.x, x), max(v.y, y), max(v.z, z))
  def minCoords(v: Vec3): Vec3 = Vec3(min(v.x, x), min(v.y, y), min(v.z, z))

  // clamp values to some value(e.g. world size)
  private def clamp(p: Double, clamp: Double): Double = if(clamp != 0 && abs(p) > clamp) clamp * (p / abs(p)) else p
  def clamp(c: Double): Unit = this.each(clamp(_, c))
  def clamp(cx: Double, cy: Double, cz: Double): Unit = setPoints(clamp(x, cx), clamp(y, cy), clamp(z, cz))

  def turnOrthogonal = {
    val inverse = 1.0 / Math.sqrt(x * x + z * z)
    Vec3(-inverse * z, y, inverse * x)
  }

  def crossProduct(v: Vec3): Vec3 = {
    val xt = z * v.y - v.z * y
    val yt = v.z * x - v.x * z
    val zt = v.x * y - v.y * x
    Vec3(xt, yt, zt)
  }

  def normalize = {
    val r = 1.0 / this.length
    this * Vec3(r, r, r)
  }

  def saturate = clampVec(0.0, 1.0)

  def clampVec(min: Double, max: Double) = Vec3(Vec3.clampRange(x, min, max),
    Vec3.clampRange(y, min, max), Vec3.clampRange(z, min, max))

  def precision: Double = .000000001

  def =~=(v: Vec3): Boolean = {
    (Math.abs(Math.abs(this.x) - Math.abs(v.x)) < precision) &&
      (Math.abs(Math.abs(this.y) - Math.abs(v.y)) < precision) &&
      (Math.abs(Math.abs(this.z) - Math.abs(v.z)) < precision)
  }

  override def toString: String = "%.2f, %.2f, %.2f".format(x,y,z)
}