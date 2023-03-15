package u02

object Positive extends App:
  
  private val positive: Int => String =_ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  private def positiveFunction(n: Int) : String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(positive(5));
  println(positiveFunction(-5));