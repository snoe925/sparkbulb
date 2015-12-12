package com.stderr.mandelbulb

import org.scalatest.FunSuite

class TestVec3 extends FunSuite {

  test("Vec3 scalar multiply") {
    val v = Vec3(0.0020052366384753057, -0.0006558835594037147, -0.9999977744189135)
    val r = v * 0.16580328008401646
    val e = Vec3(0.00033247481200385275, -0.00010874764550231575, -0.1658029110753722)
    assert(e =~= r)
  }

  test("Subtract and normalize") {
    val l = Vec3(-0.5479639334863472, 1.25, -1.1234925578739585)
    val e = Vec3(0.3099752105710803, -0.7071067811865475, 0.6355433650282366)
    assert(e =~= (Vec3() - l).normalize)
  }

  test("turn orthogonal") {
    val d = Vec3(0.2756373558169989, 0, 0.9612616959383189)
    val e = Vec3(-0.9612616959383191, 0, 0.27563735581699894)
    assert(e =~= d.turnOrthogonal)
  }

  test("dot") {
    val lightDirection = Vec3(0.3099752105710803, -0.7071067811865475, 0.6355433650282366)
    val normal = Vec3(-0.2674158164626337, -0.8446045490371423, 0.4638231741206866)
    val e = 0.8091130708858658
    assert(e === lightDirection.dot(normal))
  }
}
