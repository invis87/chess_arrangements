package chess.locating


case class ChessTable(x: Int, y: Int, fillCells: Map[Position, ChessCell] = Map.empty) {

  lazy val table: Map[Position, ChessCell] = {
    positions.map(pos => {
      val stateOption = fillCells.get(pos)
      if (stateOption.isDefined) {
        (pos, stateOption.get)
      } else {
        (pos, EmptyCell)
      }
    }).toMap
  }

  lazy val chessMap: Map[Position, ChessCell] = {
    fillCells.filter(_._2 match {
      case OccupyChessCell(_) => true
      case _ => false
    })
  }

  lazy val chessPositions: Set[Position] = {
    chessMap.keySet
  }

  lazy val positions: Set[Position] = PositioningCalc.getPositions(x, y) //todo: ugly

  lazy val emptyCells = positions.diff(fillCells.keySet)
  lazy val occupyCells = chessMap.keys

  def isCellEmpty(pos: Position): Boolean = {
    emptyCells.contains(pos)
  }

  def isOnTable(pos: Position): Boolean = positions.contains(pos)

  def onTableFilter(pos: Set[Position]): Set[Position] = {
    positions.intersect(pos)
  }

  def trySafelyAddChessman(chessman: Chessman, pos: Position): Option[ChessTable] = {
    if (isOnTable(pos) && isCellEmpty(pos)) {

      val chessmanDangerCells = chessman.getPossibleTurnMoves(pos, positions)
      if (chessmanDangerCells.intersect(chessPositions).nonEmpty) {
        return None
      }

      val newDangerCells: Map[Position, ChessCell] = emptyCells.intersect(chessmanDangerCells).map(dng => (dng, DangerousChessCell)).toMap

      return Some(ChessTable(x, y, fillCells + (pos -> OccupyChessCell(chessman)) ++ newDangerCells))
    }

    None
  }
}
