object session {

  def sqrt(x: Double) = {

    def abs(x: Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double, x: Double): Double = {
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    }

    def isGoodEnough(guess: Double, x: Double) = {
      abs(guess * guess - x) / x < 0.000000001
    }

    def improve(guess: Double, x: Double) = {
      (guess + x / guess) / 2
    }

    sqrtIter(1.0, x)
  }

}

session.sqrt(2)
session.sqrt(1e-6)
print(session.sqrt(1e60))
