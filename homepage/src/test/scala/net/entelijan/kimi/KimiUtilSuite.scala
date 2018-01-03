package net.entelijan.kimi

import org.scalatest.FunSuite

class KimiUtilSuite extends FunSuite {

  test("Test remove qualifier") {
    val is: String = KimiUtil.uqual("Es war einmal (Hallo du)")
    assert(is === "Es war einmal")
  }
  
  test("Test no remove because square brackets") {
    val is: String = KimiUtil.uqual("Es war einmal [Hallo du]")
    assert(is === "Es war einmal [Hallo du]")
  }
  
  test("Test remove qualifier with brackets in title") {
    val is: String = KimiUtil.uqual("Es (war) einmal (Hallo du)")
    assert(is === "Es (war) einmal")
  }
  
}