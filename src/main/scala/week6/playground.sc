
var word = "Playground of myself"

(for (ct <- word.toLowerCase.groupBy(c => c))
    yield ct._1 -> ct._2.length).toList.sorted

def wordOccurrences(w: String): List[(Char, Int)] = {
  for (ct <- w.toLowerCase.groupBy(c => c))
    yield ct._1 -> ct._2.length
}.toList.sorted

for {
  w <- List("word1", "word2", "word3")
} yield w

List("word1", "word2", "word3") flatten

/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: List[String]): List[(Char, Int)] = wordOccurrences(s.flatten.toString())
//  {
//    def addWord(map: Map[Char, Int], word: Word): Map[Char, Int] =
//      (wordOccurrences(word) foldLeft map)(addChar)
//    def addChar(map: Map[Char, Int], o: (Char, Int)): Map[Char, Int] =
//      map + (o._1 -> (o._2 + map(o._1) ))
//
//    (s foldLeft Map[Char, Int]())(addWord).toList
//  }

def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] = {
  if (occurrences.isEmpty) List(List())
  else {
    val tail = combinations(occurrences.tail)
    (for {
      list <- tail
      i <- 1 to occurrences.head._2
    } yield (occurrences.head._1, i) :: list) ::: tail
  }
}

combinations(List(('a', 2), ('b', 2)))

List(('a', 2), ('b', 2)).toMap
