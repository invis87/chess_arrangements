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
//      emptyCell <- table.emptyCells
//      t <- table.trySafelyAddChessman(chessmanToAdd, emptyCell)
    } yield withChess

//    def addChess(table: ChessTable, chessman: Chessman): Set[ChessTable] = {
//      chessAdded = chessAdded + 1
//      if(chessAdded % 1000 == 0) print('.')
//      table.emptyCells.flatMap(table.trySafelyAddChessman(chessman, _))
//    }

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
      notVeryDeepArrangements(nextTables.toSet, chessmans.tail)
    } else {
      halfWidthwayArrangements(nextTables.toSet, chessmans.tail, step+1)
    }
  }

  def notVeryDeepArrangements(tables: Set[ChessTable], chessmans: List[Chessman]): Set[ChessTable] = {
    if(chessmans.isEmpty) return tables

    val chessman = chessmans.head
    for {
      table <- tables
      withChess <- notVeryDeepArrangements(addChess(table, chessman), chessmans.tail)
    } yield withChess
  }

  def deepArrangements(table: ChessTable, chessmans: List[Chessman]): Iterator[ChessTable] = {
    if(chessmans.isEmpty) return Iterator(table)

    val chessman = chessmans.head
      for {
        withChess <- addChessIterator(table, chessman)
        result <- deepArrangements(withChess, chessmans.tail)
      } yield result
  }

  def serializeChessMap(chessMap : Map[Position, OccupyChessCell]): Set[Position] = {
    chessMap.map(kv => serializeChessmanPosition(kv._2.chessMan, kv._1)).toSet
  }

  def serializeChessmanPosition(chessman: Chessman, pos: Position): Position = {
    chessman match {
      case King => pos + 1000
      case Queen => pos + 800
      case Bishop => pos + 600
      case Rook => pos + 400
      case Knight => pos + 200
    }
  }

  def createFullMap(positions: Set[Position], occupyCells: Map[Position, OccupyChessCell]): Map[Position, ChessCell] = {
    val dngCells: Map[Position, ChessCell] = occupyCells.map(kv => kv._2.chessMan.getPossibleTurnMoves(kv._1, positions)).flatten.map(pos => (pos, DangerousChessCell)).toMap
    dngCells ++ occupyCells
  }

}
