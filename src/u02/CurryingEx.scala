package u02

import u02.Tuples.{Tup2, Tup4}

object CurryingEx extends App:
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

  println(compose(_ -1, _ *2) (5))

  def genericCompose[A,B, C](f: B=> C, g: A=> B): A=>C =
    x => f(g(x))
  val f : Integer => Boolean = _ >= 10
  val g : Integer => Integer = _ +9
  println(genericCompose(f, g) (1)) //true

  // Ex-6
  // 12,8 -> 8,4 -> 4,0
  // 12,3 -> 3, 0
  def gcd(a: Int, b:Int ) : Int = b match
    case b if b==0 || a<b => a
    case _ => gcd(b, a%b)


  println(gcd(12,8))
  println(gcd(14,7))
  println(gcd(10,7))

  //tail-recursive
  def gcd2(a: Int, b:Int ) : Int =
    @annotation.tailrec // checks only if optimisation is possible
    def _gcd(a: Int, b: Int): Int = b match
      case b if b == 0 || a < b => a
      case _ => _gcd(b, a % b)
    _gcd(a, b)

  println(gcd2(12, 8))
  println(gcd2(14, 7))
  println(gcd2(10,7))

  //es.7

  case class Point(x: Double, y: Double)

  def computeDistance(p1: Point, p2:Point) : Double = p1.x match
    case a if p1.x == p2.x => math.abs(p1.y - p2.y)
    case b if p1.y == p2.y => math.abs(p1.x - p2.x)
    case _ => math.sqrt(math.pow(p1.x - p2.x, 2)  + math.pow(p1.y - p2.y, 2))

  def isBetweenTwoPoints(p1: Double, p2: Double, p3: Double): Boolean =
    p3 >= p1 && p3 <= p2

  def compareX(p1: Point, p2: Point, p3: Point): Boolean = p1.x match
    case a if p1.x < p2.x => isBetweenTwoPoints(p1.x, p2.x, p3.x)
    case _  => isBetweenTwoPoints(p2.x, p1.x, p3.x)

  def compareY(p1: Point, p2: Point, p3: Point): Boolean = p1.x match
    case a if p1.y < p2.y => isBetweenTwoPoints(p1.y, p2.y, p3.y)
    case _ => isBetweenTwoPoints(p2.y, p1.y, p3.y)

  def containsPoint(p1:Point, p2:Point, p3:Point) : Boolean = p1.x match
    case a if p1.x != p2.x => compareX(p1,p2,p3)
    case _ => compareY(p1, p2, p3)
  enum Shape: // a sum type defined by enumerating various cases
    case Rectangle(points: Tup4[Point, Point, Point, Point])
    case Circle(radius: Int, center:Point)
    case Square(points:Tup4[Point, Point, Point, Point])

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(Tup4(p1,p2,p3,p4)) => 2*(computeDistance(p1, p2) + computeDistance(p2,p3))
      case Circle( r, p) => 2* math.Pi *r
      case Square(Tup4(p1,p2,p3,p4)) => 4* (computeDistance(p1,p2))

    def contains(shape: Shape, point:Point) : Boolean = shape match
      case Rectangle(Tup4(p1, p2, p3, p4)) => containsPoint(p1,p2, point) && containsPoint(p2,p3, point)
      case Circle( r, center: Point) => computeDistance(center, point) <= r
      case Square( Tup4(p1, p2, p3, p4)) => containsPoint(p1,p2, point) && containsPoint(p2,p3, point)
