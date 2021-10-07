package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double]
  ): Signal[Double] =
    Signal(b() * b() - 4 * a() * c())

  def computeSolutions(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double],
      delta: Signal[Double]
  ): Signal[Set[Double]] =
    Signal(
      delta() match {
        case n if n > 0 =>
          Set(
            (-b() + Math.sqrt(n)) / (2 * a()),
            (-b() - Math.sqrt(n)) / (2 * a())
          )
        case n if n == 0 =>
          Set(
            -b() / (2 * a())
          )
        case n if n < 0 => Set()

      }
    )
