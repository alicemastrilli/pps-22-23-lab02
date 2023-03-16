package u02

object Optionals extends App:

  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:

    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(f: A=>Boolean):Option[A] = opt match
      case Some(a) if f(a) => Some(a)
      case _ => None()

    def map[A, B](opt: Option[A])(f: A=> B) : Option[B] = opt match
      case Some(a) => Some(f.apply(a))
      case _ => None()

    def fold[A,B](opt: Option[A])(default: B)(f: A=>B) : B = opt match
      case Some(a) => f(a)
      case _ => default

  import Option.*

  val s1: Option[Int] = Some(1)
  val s2: Option[Int] = Some(2)
  val s3: Option[Int] = None()

  println(s1) // Some(1)
  println(orElse(s1, 0))
  println(orElse(s3, 0)) // 1,0
  println(flatMap(s1)(i => Some(i + 1))) // Some(2)
  println(flatMap(s1)(i => flatMap(s2)(j => Some(i + j)))) // Some(3)
  println(flatMap(s1)(i => flatMap(s3)(j => Some(i + j)))) // None

  println(filter(Some(5))(_ > 2)) //Some(5)
  println(filter(Some(10))(_ % 2==0)) //Some(10)
  println(filter(Some(5))(_ > 8) ) //None
  println(filter(None[Int]())(_ > 2)) //None

  println(map(Some(5))(_ > 2)) // Some(true)
  println(map(Some(5))(_ > 8)) // Some(false)
  println(map(Some(5))(_ + 8)) // Some(13)
  println(map(None[Int]())(_ > 2)) // None

  println(fold(Some(5))(1)(_ + 1)) // 6
  println(fold (None[Int]())(1)(_ + 1)) // 1
  println(fold(Some(5))(true)(_ < 1)) // false
