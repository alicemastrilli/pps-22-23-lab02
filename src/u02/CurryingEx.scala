package u02

object CurryingEx extends App {
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


}
