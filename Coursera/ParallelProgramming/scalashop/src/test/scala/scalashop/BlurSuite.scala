package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")

    assert(boxBlurKernel(src, 1, 3, 1) === 16,
      s"(boxBlurKernel(1, 3, 1) should be 16, " +
        s"but it's ${boxBlurKernel(src, 1, 3, 1)})")

    assert(boxBlurKernel(src, 0, 1, 1) === 3,
      s"(boxBlurKernel(0, 1, 1) should be 3, " +
        s"but it's ${boxBlurKernel(src, 0, 1, 1)})")
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 3, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 3)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 4)
    check(dst, 0, 2, 5)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 6)
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the 3x3 image correctly") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 3)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 4)
    check(dst, 0, 2, 0)
    check(dst, 1, 2, 0)
    check(dst, 2, 2, 0)
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the 4x4 image correctly") {
    val w = 4
    val h = 4
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 3
    src(0, 1) = 4; src(1, 1) = 5; src(2, 1) = 6; src(3, 1) = 7
    src(0, 2) = 8; src(1, 2) = 9; src(2, 2) = 10; src(3, 2) = 11
    src(0, 3) = 12; src(1, 3) = 13; src(2, 3) = 14; src(3, 3) = 15

    HorizontalBoxBlur.blur(src, dst, 0, 4, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 3)
    check(dst, 2, 0, 4)
    check(dst, 3, 0, 4)
    check(dst, 0, 1, 4)
    check(dst, 1, 1, 5)
    check(dst, 2, 1, 6)
    check(dst, 3, 1, 6)
    check(dst, 0, 2, 8)
    check(dst, 1, 2, 9)
    check(dst, 2, 2, 10)
    check(dst, 3, 2, 10)
    check(dst, 0, 3, 10)
    check(dst, 1, 3, 11)
    check(dst, 2, 3, 12)
    check(dst, 3, 3, 12)
  }

  test("HorizontalBoxFlur.parBlur with radius 0 and task 3 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 3, 0)

    check(dst, 0, 0, 0)
    check(dst, 1, 0, 1)
    check(dst, 2, 0, 2)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 5)
    check(dst, 0, 2, 6)
    check(dst, 1, 2, 7)
    check(dst, 2, 2, 8)
  }

  test("HorizontalBoxFlur.parBlur with radius 1 and task 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 1, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 3)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 4)
    check(dst, 0, 2, 5)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 6)
  }

  test("HorizontalBoxFlur.parBlur with radius 1 and task 3 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 3, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 3)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 4)
    check(dst, 0, 2, 5)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 6)
  }

  test("HorizontalBoxFlur.parBlur with radius 1 and task 2 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 2, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 3)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 4)
    check(dst, 0, 2, 5)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 6)
  }

  test("HorizontalBoxFlur.parBlur with radius 1 and task 4 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 4, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 3)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 4)
    check(dst, 0, 2, 5)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 6)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 and task 1 should correctly blur the 4x4 image correctly") {
    val w = 4
    val h = 4
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 3
    src(0, 1) = 4; src(1, 1) = 5; src(2, 1) = 6; src(3, 1) = 7
    src(0, 2) = 8; src(1, 2) = 9; src(2, 2) = 10; src(3, 2) = 11
    src(0, 3) = 12; src(1, 3) = 13; src(2, 3) = 14; src(3, 3) = 15

    HorizontalBoxBlur.parBlur(src, dst, 2, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 3)
    check(dst, 2, 0, 4)
    check(dst, 3, 0, 4)
    check(dst, 0, 1, 4)
    check(dst, 1, 1, 5)
    check(dst, 2, 1, 6)
    check(dst, 3, 1, 6)
    check(dst, 0, 2, 8)
    check(dst, 1, 2, 9)
    check(dst, 2, 2, 10)
    check(dst, 3, 2, 10)
    check(dst, 0, 3, 10)
    check(dst, 1, 3, 11)
    check(dst, 2, 3, 12)
    check(dst, 3, 3, 12)
  }

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    check(dst, 0, 0, 4)
    check(dst, 1, 0, 5)
    check(dst, 2, 0, 5)
    check(dst, 3, 0, 6)
    check(dst, 0, 1, 4)
    check(dst, 1, 1, 5)
    check(dst, 2, 1, 5)
    check(dst, 3, 1, 6)
    check(dst, 0, 2, 4)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 5)
    check(dst, 3, 2, 6)
  }

  def check(dst: Img, x: Int, y: Int, expected: Int) =
    assert(dst(x, y) == expected,
      s"(destination($x, $y) should be $expected)")
}
