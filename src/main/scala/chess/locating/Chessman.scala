package chess.locating

trait Chessman {

  protected val straightMoves: Set[Position] = Set(
    Position(1, 0),
    Position(0, 1),
    Position(-1, 0),
    Position(0, -1)
  )

  protected val diagonalMoves: Set[Position] = Set(
    Position(1, 1),
    Position(-1, 1),
    Position(-1, -1),
    Position(1, -1)
  )

  protected val horseMoves: Set[Position] = Set(
    Position(2, 1),
    Position(2, -1),
    Position(-2, 1),
    Position(-2, -1),
    Position(1, 2),
    Position(1, -2),
    Position(-1, 2),
    Position(-1, -2)
  )

  protected def oneStepMovePattern: Set[Position]

  def oneTurnMoves: Stream[Set[Position]]

  protected def infiniteTurnMove: Stream[Set[Position]] = {
    oneStepMovePattern #:: Stream.from(2).map(cnt => oneStepMovePattern.map(_ * cnt))
  }

  def getPossibleTurnMoves(initPos: Position, tablePositions: Set[Position]): Set[Position] = {
    if(tablePositions.contains(initPos)) {

      return oneTurnMoves.map(moves => tablePositions.intersect(moves.map(_ + initPos))).takeWhile(_.nonEmpty).flatten.toSet
    }

    Set.empty
  }

  def sign: Char
}

object King extends Chessman {
  override protected def oneStepMovePattern: Set[Position] = straightMoves ++ diagonalMoves

  override def oneTurnMoves: Stream[Set[Position]] =
    Stream(oneStepMovePattern)

  override def sign: Char = 'K'
}

object Knight extends Chessman {
  override protected def oneStepMovePattern: Set[Position] = horseMoves

  override def oneTurnMoves: Stream[Set[Position]] =
    Stream(oneStepMovePattern)

  override def sign: Char = 'N'
}

object Queen extends Chessman {
  override protected def oneStepMovePattern: Set[Position] = straightMoves ++ diagonalMoves

  override def oneTurnMoves: Stream[Set[Position]] = infiniteTurnMove

  override def sign: Char = 'Q'
}

object Bishop extends Chessman {
  override protected def oneStepMovePattern: Set[Position] = diagonalMoves

  override def oneTurnMoves: Stream[Set[Position]] = infiniteTurnMove

  override def sign: Char = 'B'
}

object Rook extends Chessman {
  override protected def oneStepMovePattern: Set[Position] = straightMoves

  override def oneTurnMoves: Stream[Set[Position]] = infiniteTurnMove

  override def sign: Char = 'R'
}
