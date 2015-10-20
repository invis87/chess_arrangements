package chess.locating

import scala.collection.immutable.ListMap

object Main {

  def main(args: Array[String]) = {

    val (x, y) = (6, 9)
    val chess = new ChessTable(x, y)
//    val chessmans = List(Knight, Knight, Knight, Knight, Rook, Rook)
    val chessmans = List(King, King, Queen, Bishop, Rook, Knight)


//    val widthwaySolution = PositioningCalc.widthwayArrangements(Set(chess), chessmans)
//    println(s"widthway solution size: ${widthwaySolution.size}")
//
//
//    val halfWidthwaySolution = PositioningCalc.halfWidthwayArrangements(Set(chess), chessmans)
//    println(s"halfWidthway solution size: ${halfWidthwaySolution.size}")


    val deepSolution = PositioningCalc.deepArrangements(Iterator(chess), chessmans)

    val resultDeepSet = scala.collection.mutable.Set[Map[Position, ChessCell]]()
    deepSolution.foreach(table => {
      if (!resultDeepSet.contains(table.chessMap)) {
        resultDeepSet += table.chessMap
      }
    })
    println(s"\ndeepSolution set size: ${resultDeepSet.size}")

    resultDeepSet.take(20).foreach(map => println(
      ChessDrawer.draw(
        ChessTable(x, y,
          PositioningCalc.createFullMap(
            PositioningCalc.getPositions(x, y), map.map(kv => (kv._1, kv._2.asInstanceOf[OccupyChessCell]))))) + "\n------\n"))

    //        printTestTable()
  }

  def printTestTable() = {
    val chess = new ChessTable(20, 20)

    val toAdd = ListMap(
      Bishop -> Position(3, 3),
      Knight -> Position(3, 13),
      Queen -> Position(14, 8),
      King -> Position(3, 6),
      Rook -> Position(10, 1))

    val filledChess = add(Some(chess), toAdd)
    println(ChessDrawer.draw(filledChess.get))

  }

  def add(table: Option[ChessTable], chessmans: Map[Chessman, Position]): Option[ChessTable] = {
    if (chessmans.isEmpty) return table

    val withChessman = table.flatMap(t => t.trySafelyAddChessman(chessmans.head._1, chessmans.head._2))
    if (withChessman.isEmpty) {
      return table
    }

    add(withChessman, chessmans.tail)
  }
}
