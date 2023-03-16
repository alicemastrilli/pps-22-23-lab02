package u02

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test
import u02.CurryingEx.Point
import u02.CurryingEx.Shape.*
import u02.Tuples.Tup4


class ExerciseTest:
  val rect1 = Rectangle(Tup4(Point(-1.0, 3.0),Point(2.0, 3.0),Point(2.0, -3.0),Point(-1.0, -3.0)))
  val rect2=  Rectangle(Tup4(Point(-3.0, 1.0),Point(3.0, 1.0),Point(3.0, -1.0),Point(-3.0, -1.0)))
  val circle1 = Circle(2, Point(6.0, 0.0))
  val circle2 = Circle(3, Point(4.0, 6.0))
  val square1 = Square(Tup4(Point(-2.0, 4.0), Point(4.0, 4.0), Point(4.0, -2.0), Point(-2.0, -2.0)))
  val square2 = Square(Tup4(Point(3.0, -1.0), Point(6.0, -1.0), Point(6.0, 2.0), Point(3.0, 2.0)))
  val point1 = Point(3.5, 3.5)
  val point2 = Point(-2.0, 0.0)
  val point3 = Point(4.0, 0.0)
  val point4 = Point(1.0, 1.0)
  @Test def testPerimeter() =
    assertEquals(18, perimeter(rect1))
    assertEquals(16, perimeter(rect2))
    assertEquals(12.6, Math.round(perimeter(circle1)*10.0)/10.0);
    assertEquals(18.8, Math.round(perimeter(circle2)*10.0)/10.0);
    assertEquals(24, perimeter(square1))
    assertEquals(12, perimeter(square2))

  @Test def testContainsPoint() =
    assertFalse(contains(rect1, point1))
    assertFalse(contains(rect1, point2))
    assertFalse(contains(rect1, point3))
    assertTrue(contains(rect1, point4))
    assertFalse(contains(rect2, point1))
    assertTrue(contains(rect2, point2))
    assertFalse(contains(rect2, point3))
    assertTrue(contains(rect2, point4))

    assertFalse(contains(circle1, point1))
    assertFalse(contains(circle1, point2))
    assertTrue(contains(circle1, point3))
    assertFalse(contains(circle1, point4))
    assertTrue(contains(circle2, point1))
    assertFalse(contains(circle2, point2))
    assertFalse(contains(circle2, point3))
    assertFalse(contains(circle2, point4))

    assertTrue(contains(square1, point1))
    assertTrue(contains(square1, point2))
    assertTrue(contains(square1, point3))
    assertTrue(contains(square1, point4))
    assertFalse(contains(square2, point1))
    assertFalse(contains(square2, point2))
    assertTrue(contains(square2, point3))
    assertFalse(contains(square2, point4))



