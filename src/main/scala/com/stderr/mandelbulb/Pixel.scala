package com.stderr.mandelbulb

case class Pixel(val red: Long, val green: Long, val blue: Long) extends Ordered[Pixel] {
  override def compare(that: Pixel): Int = {
    val dr = this.red - that.red
    val dg = this.green - that.green
    val db = this.blue - that.blue
    if (dr < 0) -1 else {
      if (dg < 0) -1 else {
        if (db < 0) -1 else {
          if (db > 0) 1 else 0
        }
      }
    }
  }
}