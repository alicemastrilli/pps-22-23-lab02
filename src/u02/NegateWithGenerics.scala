package u02

object NegateWithGenerics extends App{
  def neg[X] (predicate: X => Boolean) : (X => Boolean) =
    !predicate(_)

  val even: Int => Boolean = _ % 2 == 0 // predicate on strings
  val odd = neg(even) // which type of notEmpty?
  println(odd(1)) // true
  println(odd(12)) // false
  println(odd(5) && !even(7)) //true

}
