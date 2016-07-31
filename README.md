# Chess arrangements
Calculate the number of distinct layouts for which all _chessmans_ can be placed on the chessboard without threatening each other.

### Example of usage
```
val chess = new ChessTable(6, 6)
val chessmans = List(King, Queen, Bishop, Rook, Knight)

val resultDeepSet = scala.collection.mutable.Set[Map[Position, ChessCell]]()

val resultDeep = PositioningCalc.deepArrangements(chess, chessmans)
resultDeep.foreach(table => {
      if (!resultDeepSet.contains(table.chessMap)) {
        resultDeepSet += table.chessMap
      }})
println(s"\nresultDeep set size: ${resultDeepSet.size}")
```
One of the results:
* o - "hot" points
* _ - sage points
* N - Knight
* R - Rook
* B - bishop
* Q - Queen
```
o _ o _ o N
o o o o R o
_ _ o _ o o
o _ o B o o
_ o o o o K
o o Q o o o
```
