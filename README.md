# Chess arrangements
Calculate the number of distinct layouts for which all _chessmans_ can be placed on the chessboard without threatening each other.

### Example of usage
```
val chess = new ChessTable(6, 9)
val chessmans = List(King, King, Queen, Bishop, Rook, Knight)

val resultDeepSet = scala.collection.mutable.Set[Map[Position, ChessCell]]()

val resultDeep = PositioningCalc.deepArrangements(chess, chessmans)
resultDeep.foreach(table => {
      if (!resultDeepSet.contains(table.chessMap)) {
        resultDeepSet += table.chessMap
      }})
println(s"\nresultDeep set size: ${resultDeepSet.size}")
```
