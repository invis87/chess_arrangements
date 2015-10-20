package chess.locating

case class Position(x: Int, y: Int) extends Ordered[Position] {

  def toPair: (Int, Int) = (x, y)

  def +(that: Position): Position = Position(x + that.x, y + that.y)

  def +(i: Int): Position = Position(x + i, y + i)

  def -(that: Position): Position = Position(x - that.x, y - that.y)

  def *(i: Int): Position = Position(x * i, y * i)

  override def toString: String = s"($x, $y)"

  import scala.math.Ordered.orderingToOrdered
  override def compare(that: Position): Int = (y, x) compare (that.y, that.x)
}
