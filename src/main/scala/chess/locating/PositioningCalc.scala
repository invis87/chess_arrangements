package chess.locating

import scala.annotation.tailrec

object PositioningCalc {

  val positionsMap = scala.collection.mutable.Map[(Int, Int), Set[Position]]()

  def getPositions(x: Int, y: Int): Set[Position] = {
    if(!positionsMap.contains((x, y))) {
      val positions = for {
        x <- 0 until x
        y <- 0 until y
        } yield Position(x, y)

      positionsMap((x, y)) = positions.toSet
    }

    positionsMap((x, y))
  }

  @tailrec def widthwayArrangements(tables: Set[ChessTable], chessmans: List[Chessman], level: Int = 0): Set[ChessTable] = {
    if(chessmans.isEmpty) return tables

    val chessmanToAdd = chessmans.head
    val tablesWithChess = for {
      table <- tables
      withChess <- addChess(table, chessmanToAdd)
    } yield withChess

    widthwayArrangements(tablesWithChess, chessmans.tail)
  }

  @tailrec def widthwayArrangementsFoldImplm(tables: Set[ChessTable], chessmans: List[Chessman], level: Int = 0): Set[ChessTable] = {
    if(chessmans.isEmpty) return tables

    val chessman = chessmans.head

    val nextTables = tables.foldLeft(scala.collection.mutable.Set.empty[ChessTable])((set, table) => set ++= addChess(table, chessman))
    println(nextTables.size)

    widthwayArrangementsFoldImplm(nextTables.toSet, chessmans.tail)
  }

  var chessAdded = 0
  def addChess(table: ChessTable, chessman: Chessman): Set[ChessTable] = {
    chessAdded = chessAdded + 1
    if(chessAdded % 1000 == 0) print('.')
    table.emptyCells.flatMap(table.trySafelyAddChessman(chessman, _))
  }

  def addChessIterator(table: ChessTable, chessman: Chessman): Iterator[ChessTable] = {
    chessAdded = chessAdded + 1
    if(chessAdded % 1000 == 0) print('.')
    table.emptyCells.toIterator.flatMap(table.trySafelyAddChessman(chessman, _))
  }

  @tailrec def halfWidthwayArrangements(tables: Set[ChessTable], chessmans: List[Chessman], step: Int = 0): Set[ChessTable] = {
    if(chessmans.isEmpty) return tables

    val chessman = chessmans.head

    val nextTables = tables.foldLeft(scala.collection.mutable.Set.empty[ChessTable])((set, table) => set ++= addChess(table, chessman))
    println(nextTables.size)

    if(step == chessmans.size) {
      deepArrangements(nextTables.toIterator, chessmans.tail).toSet
    } else {
      halfWidthwayArrangements(nextTables.toSet, chessmans.tail, step+1)
    }
  }

  def deepArrangements(tables: Iterator[ChessTable], chessmans: List[Chessman]): Iterator[ChessTable] = {
    if(chessmans.isEmpty) return tables

    val chessman = chessmans.head
    for {
      table <- tables
      withChess <- deepArrangements(addChessIterator(table, chessman), chessmans.tail)
    } yield withChess
  }

  def createFullMap(positions: Set[Position], occupyCells: Map[Position, OccupyChessCell]): Map[Position, ChessCell] = {
    val dngCells: Map[Position, ChessCell] = occupyCells.map(kv => kv._2.chessMan.getPossibleTurnMoves(kv._1, positions)).flatten.map(pos => (pos, DangerousChessCell)).toMap
    dngCells ++ occupyCells
  }

}
