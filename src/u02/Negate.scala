package u02

object Negate extends App{

  private val neg: (String => Boolean) => (String =>Boolean) =
    f => (i => !f(i));

  private def negMethod (predicate: (String => Boolean)) : (String => Boolean) =
    !predicate(_)


  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = negMethod(empty) // which type of notEmpty?
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty(""))

}
