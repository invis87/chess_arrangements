package chess.locating


object ChessDrawer {

  def draw(chess: ChessTable): String  = {

    val positionMap = chess.table.toList.sortBy(_._1)

    val xDimension = positionMap.map(_._1).max.x + 1

    val strBld = new StringBuilder
    positionMap.map(_._2).grouped(xDimension).toList.map(_ mkString " ").foreach(strBld.append(_).append("\n"))

    strBld.toString()
  }

}
