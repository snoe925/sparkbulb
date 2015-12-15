package com.stderr.mandelbulb

case class Point(val x: Int, val y: Int) extends Ordered[Point] {
  override def compare(that: Point): Int = if (this.y > that.y) 1 else {
    if (this.y == that.y) this.x - that.x else -1
  }
}