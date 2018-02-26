package ppWeek3

object Mandelbrot {

  def color(i: Int): Int = ???

  private def computePixel(xc: Double, yc: Double, maxIt: Int): Int = {
    var i = 0
    var x, y = 0.0
    while (x * x + y * y < 4 && i < maxIt) {
      val xt = x * x - y * y + xc
      val yt = 2 * x * y + yc
      x = xt
      y = yt
      i += 1
    }
    color(i)
  }

}
