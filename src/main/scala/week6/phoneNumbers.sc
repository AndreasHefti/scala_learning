import scala.io.Source

/* more exercises:
 * Convert phone numbers to sentences:
 */

val mnemonics = Map(
  '2' -> "ABC",
  '3' -> "DEF",
  '4' -> "GHI",
  '5' -> "JKL",
  '6' -> "MNO",
  '7' -> "PQRS",
  '8' -> "TUV",
  '9' -> "WXYZ"
)

val in = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
val words = in.getLines.toList filter (w => w forall (c => c isLetter) )

//val charCode: Map[Char, Char] = mnemonics.foldLeft(Map[Char, Char]())(invertAndFlattenMnemonics)
//def invertAndFlattenMnemonics(map: Map[Char, Char], entry: (Char, String)): Map[Char, Char] = {
//  map ++ (for (c <- entry._2) yield c -> entry._1)
//}
val charCode: Map[Char, Char] =
  for {
    (digit, str) <- mnemonics
    ltr <- str
  } yield ltr -> digit

// NOTE: usually a fold can be written as a for expression and a fold with a function that uses
//       a for expression can be written as a for expression with two iterations on two variables
//       the yield produces a Collection of arbitrary type; Map in the case of a -> b


def wordCode(word: String): String =
  //word map (c => charCode(c.toUpper))
  // NOTE: this is possible because Map also works as a function with
  // in this case: apply(c: Char): Char
  word.toUpperCase map charCode

wordCode("Java")

val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet

encode("7225247386")

def translate(number: String): List[String] =
  encode(number) map (_ mkString " ") toList

translate("7225247386")