package chess.locating

trait ChessCell {
  def sign: Char

  override def toString: String = sign.toString
}

object EmptyCell extends ChessCell {
  override def sign: Char = '_'
}

object DangerousChessCell extends ChessCell {
  override def sign: Char = 'o'
}

case class OccupyChessCell(chessMan: Chessman) extends ChessCell {
  override def sign: Char = chessMan.sign
}


