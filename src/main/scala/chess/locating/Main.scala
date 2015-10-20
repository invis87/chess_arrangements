package chess.locating

import scala.collection.immutable.ListMap

object Main {
  var i = 0
  val resultDeepSet = scala.collection.mutable.Set[Map[Position, ChessCell]]()

  def main(args: Array[String]) = {

    val (x, y) = (6, 9)
    val chess = new ChessTable(x, y)
//        val chessmans = List(Knight, Knight, Rook, Rook, Knight, Knight)


    val chessmans = List(King, King, Queen, Bishop, Rook, Knight)


    //    val result = PositioningCalc.widthwayArrangements(Set(chess), chessmans)
    //    println(s"Origin solution: ${result.size}")


    //    val resultV2Stream = PositioningCalc.allPossibleArrangmentsV2(chess, chessmans)
    //    val resultV2Set = resultV2Stream.toSet
    //    val resultV2List = resultV2Stream.toList
    //
    //    val x = 0
    //    println(s"Old: ${x}! NewSet: ${resultV2Set.size}, newList: ${resultV2List.size}")


    //    val resultV3 = PositioningCalc.halfWidthwayArrangements(Set(chess), chessmans)
    //    println(s"V3 set size: ${resultV3.size}")
    //
    //
    val resultDeep = PositioningCalc.deepArrangements(chess, chessmans)


    resultDeep.foreach(table => {
      if (!resultDeepSet.contains(table.chessMap)) {
        resultDeepSet += table.chessMap
      }
    }
    )
    //    val resultDeepSet = resultDeep.foldLeft(scala.collection.mutable.Set[ChessTable]())((set, table) => set += table)
    println(s"\nresultDeep set size: ${resultDeepSet.size}")

    resultDeepSet.take(20).foreach(map => println(
      ChessDrawer.draw(
        ChessTable(x, y,
          PositioningCalc.createFullMap(
            PositioningCalc.getPositions(x, y), map.map(kv => (kv._1, kv._2.asInstanceOf[OccupyChessCell]))))) + "\n------\n"))


    //    val resultDeep = PositioningCalc.deepArrangementsOnlyCount(Set(chess), chessmans)
    //    println(s"resultDeep set size: ${resultDeep }")


    //
    //    resultV2.take(20).foreach(t => if(t.isDefined) {println(ChessDrawer.draw(t.get) + "------------")})

    //    println(ChessDrawer.draw(PositioningCalc.placeChessmans(Some(chess), chessmans).get))

    //    val tables = PositioningCalc.allPossibleArrangments(chess, chessmans)
    //    println(tables.size)
    //
    //    println(s"Distinct layouts for table(${chess.x}, ${chess.y}) and chessmans(${chessmans.map(_.sign) mkString ", "}) = ${tables.size}")

    //    println("\nTables:")
    //    tables.foreach(t => println(ChessDrawer.draw(t) + "------------"))

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
