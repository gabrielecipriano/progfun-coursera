object session {
  1 + 3

  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double) = {
    abs(x * x - x) < 0.01
  }

  def improve(guess: Double, x: Double) = {
    (guess + x / guess) / 2
  }

  def sqrt(x: Double) = sqrtIter(1.0, x)

  sqrt(2)

}