class Shape(val x: Int, val y: Int)

object Shape {
  def apply(x: Int, y: Int): Shape = new Shape(x, y)
}
