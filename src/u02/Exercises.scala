package u02

import u02.Exercises.Option.*
import u02.Exercises.Shape.*
import u02.ProductTypes.Point2D
import math.*
import u02.Tuples.{Tup2, Tup4}

object Exercises extends App:
  //TASK 2, svolto da solo
  //Ex. 3_a
  private val positive: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  private def positiveFunction(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println("Ex. 3_a")
  println(positive(5)); //"positive"
  println(positiveFunction(-5));  //"negative"

  //Ex 3_b
  private val neg: (String => Boolean) => (String => Boolean) =
    f => (i => !f(i));

  private def negMethod(predicate: (String => Boolean)): (String => Boolean) =
    !predicate(_)

  println("Ex 3_b")
  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = negMethod(empty)
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty("")) //true
  val notEmpty2 = neg(empty)
  println(notEmpty2("foo")) // true
  println(notEmpty2("")) // false
  println(notEmpty2("foo") && !notEmpty("")) //true

  // Ex 3_c
  def neg[X](predicate: X => Boolean): (X => Boolean) =
    !predicate(_)
  println("Ex. 3_c")
  val even: Int => Boolean = _ % 2 == 0 // predicate on strings
  val odd = neg(even) // which type of notEmpty?
  println(odd(1)) // true
  println(odd(12)) // false
  println(odd(5) && !even(7)) //true

  // Ex.4
  //no curryng
  val p1 : (Integer, Integer, Boolean) => Boolean = (x,y,z) => z match
    case z if x<=y => true
    case _ => false
  //currying
  val p2: Integer => Integer => Boolean => Boolean = x=> y => z => z match
    case z if x <= y => true
    case _ => false

  // no currying
  def p3(x: Integer, y:Integer, z:Boolean): Boolean = (x<=y == z)

  //currying
  def p4 (x: Integer) (y: Integer) (z: Boolean) : Boolean = x<=y == z

  println("Ex. 4")
  println(p1(4,5,true)) //true
  println(p1(4, 3, true)) //false
  println(p2(4)(5)(true)) //true
  println(p2(4)(3)(true)) //false
  println(p3(4, 5, true)) //true
  println(p3(4, 3, true)) //false
  println(p4(4)(5)(true)) //true
  println(p4(4)(3)(true)) //false


  // Ex.5
  def compose(f: Int => Int, g:Int => Int) : Int => Int =
    x => f(g(x))
  println("Ex. 5")
  println(compose(_ -1, _ *2) (5)) //9
  println(compose(_ *10, _ +2) (7)) //90


  def genericCompose[A,B, C](f: B=> C, g: A=> B): A=>C =
    x => f(g(x))
  val f : Integer => Boolean = _ >= 10
  val g : Integer => Integer = _ +9
  println(genericCompose(f, g) (1)) //true

  //TASK 3, svolto da solo
  // Ex.6
  def gcd(a: Int, b:Int ) : Int = a match
    case a if b==0 => a
    case _ => gcd(min(a,b), max(a,b)% min(a,b))

  println("Ex.6")
  println(gcd(12,8)) // 4
  println(gcd(14,7)) //7
  println(gcd(10,7)) //1
  println(gcd(7,8)) //1
  println(gcd(4,4)) //4
  //tail-recursive
  def trGcd(a: Int, b:Int ) : Int =
    @annotation.tailrec // checks only if optimisation is possible
    def _gcd(a: Int, b: Int): Int = a match
      case a if b == 0 => a
      case _ => _gcd(min(a, b), max(a, b) % min(a, b))
    _gcd(a, b)

  println("Ex.6 - tail recursive")
  println(trGcd(12, 8)) // 4
  println(trGcd(14, 7)) //7
  println(trGcd(10, 7)) //1
  println(trGcd(7, 8)) //1
  println(trGcd(4, 4)) //4

  //TASK 4, svolto da solo
  //es.7
  def computeDistance(p1: Point2D, p2:Point2D) : Double =
    sqrt(pow(p1.x - p2.x, 2)  + pow(p1.y - p2.y, 2))

  def isBetweenTwoPoints(p1: Double, p2: Double, p3: Double): Boolean =
    p3 >= p1 && p3 <= p2

  def compareX(p1: Point2D, p2: Point2D, p3: Point2D): Boolean = p1.x match
    case a if p1.x < p2.x => isBetweenTwoPoints(p1.x, p2.x, p3.x)
    case _  => isBetweenTwoPoints(p2.x, p1.x, p3.x)

  def compareY(p1: Point2D, p2: Point2D, p3: Point2D): Boolean = p1.x match
    case a if p1.y < p2.y => isBetweenTwoPoints(p1.y, p2.y, p3.y)
    case _ => isBetweenTwoPoints(p2.y, p1.y, p3.y)

  def containsPoint(p1:Point2D, p2:Point2D, p3:Point2D) : Boolean = p1.x match
    case a if p1.x != p2.x => compareX(p1,p2,p3)
    case _ => compareY(p1, p2, p3)
  enum Shape: // a sum type defined by enumerating various cases
    case Rectangle(points: Tup4[Point2D, Point2D, Point2D, Point2D])
    case Circle(radius: Int, center:Point2D)
    case Square(points:Tup4[Point2D, Point2D, Point2D, Point2D])


  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(Tup4(p1,p2,p3,p4)) => 2*(computeDistance(p1, p2) + computeDistance(p2,p3))
      case Circle( r, p) => 2* Pi *r
      case Square(Tup4(p1,p2,p3,p4)) => 4* (computeDistance(p1,p2))

    def contains(shape: Shape, point:Point2D) : Boolean = shape match
      case Rectangle(Tup4(p1, p2, p3, p4)) => containsPoint(p1,p2, point) && containsPoint(p2,p3, point)
      case Circle( r, center: Point2D) => computeDistance(center, point) <= r
      case Square( Tup4(p1, p2, p3, p4)) => containsPoint(p1,p2, point) && containsPoint(p2,p3, point)


  val rect1 = Rectangle(Tup4(Point2D(-1.0, 3.0), Point2D(2.0, 3.0), Point2D(2.0, -3.0), Point2D(-1.0, -3.0)))
  val rect2 = Rectangle(Tup4(Point2D(-3.0, 1.0), Point2D(3.0, 1.0), Point2D(3.0, -1.0), Point2D(-3.0, -1.0)))
  val circle1 = Circle(2, Point2D(6.0, 0.0))
  val circle2 = Circle(3, Point2D(4.0, 6.0))
  val square1 = Square(Tup4(Point2D(-2.0, 4.0), Point2D(4.0, 4.0), Point2D(4.0, -2.0), Point2D(-2.0, -2.0)))
  val square2 = Square(Tup4(Point2D(3.0, -1.0), Point2D(6.0, -1.0), Point2D(6.0, 2.0), Point2D(3.0, 2.0)))
  val point1 = Point2D(3.5, 3.5)
  val point2 = Point2D(-2.0, 0.0)
  val point3 = Point2D(4.0, 0.0)
  val point4 = Point2D(1.0, 1.0)

  println("Ex.7")

  println(perimeter(rect1)) //18
  println(perimeter(rect2)) //16
  println(perimeter(circle1)) //12.6
  println(perimeter(circle2)) //18.8
  println(perimeter(square1)) //24
  println(perimeter(square2)) //12

  println(contains(rect1, point1)) //false
  println(contains(rect1, point2)) //false
  println(contains(rect1, point3)) //false
  println(contains(rect1, point4)) //true
  println(contains(rect2, point1)) //false
  println(contains(rect2, point2)) //true
  println(contains(rect2, point3)) //false
  println(contains(rect2, point4)) //true

  println(contains(circle1, point1)) //false
  println(contains(circle1, point2)) //false
  println(contains(circle1, point3)) //true
  println(contains(circle1, point4)) //false
  println(contains(circle2, point1)) //true
  println(contains(circle2, point2)) //false
  println(contains(circle2, point3)) //false
  println(contains(circle2, point4)) //false

  println(contains(square1, point1)) //true
  println(contains(square1, point2)) //true
  println(contains(square1, point3)) //true
  println(contains(square1, point4)) //true
  println(contains(square2, point1)) //false
  println(contains(square2, point2)) //false
  println(contains(square2, point3)) //true
  println(contains(square2, point4)) //false


  //TASK 5, svolto da solo
  //ex.8
  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:
    def filter[A](opt: Option[A])(f: A => Boolean): Option[A] = opt match
      case Some(a) if f(a) => Some(a)
      case _ => None()

    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt match
      case Some(a) => Some(f.apply(a))
      case _ => None()

    def fold[A, B](opt: Option[A])(default: B)(f: A => B): B = opt match
      case Some(a) => f(a)
      case _ => default

  println("Ex.8")

  println(filter(Some(5))(_ > 2)) //Some(5)
  println(filter(Some(10))(_ % 2 == 0)) //Some(10)
  println(filter(Some(5))(_ > 8)) //None
  println(filter(None[Int]())(_ > 2)) //None

  println(map(Some(5))(_ > 2)) // Some(true)
  println(map(Some(5))(_ > 8)) // Some(false)
  println(map(Some(5))(_ + 8)) // Some(13)
  println(map(None[Int]())(_ > 2)) // None

  println(fold(Some(5))(1)(_ + 1)) // 6
  println(fold(None[Int]())(1)(_ + 1)) // 1
  println(fold(Some(5))(true)(_ < 1)) // false