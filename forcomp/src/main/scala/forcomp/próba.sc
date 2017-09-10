val x: String = "aba"

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]
def wordOccurrences(w: Word): Occurrences = {
  val groupedWords = w.toLowerCase groupBy (x => x)
  val pairs = groupedWords map { case (char, list) => (char, list.length) }
  pairs.toList sortWith (_._1 < _._1)
}

val s = List(x,x)
//def sentenceOccurrences(s: Sentence): Occurrences = {
  val lista = s.flatMap(wordOccurrences)
  val mapa = lista groupBy (_._1)
  val mapa2 =mapa map { case (char, list) => (char, (list.unzip._2).sum) }
  mapa2.toList  sortWith (_._1 < _._1)
//}
//sentenceOccurrences(s)
val dictionary = List("ate","tea","eat")

val code = dictionary map wordOccurrences
val pairs = dictionary zip code
val grouped = pairs groupBy(_._2)
val maped = grouped map { case (chars,list) => (chars, list.unzip._1) }

val y = List(('b',1),('c',1))

def combinations(occurrences: Occurrences): List[Occurrences] = {
  if (occurrences.isEmpty) List(List())
  else {
    for {
      (a,b) <- occurrences
      i <- (0 to b)
      sth <- combinations(occurrences.tail)
    } yield if (i > 0) (a, i) :: sth else sth
  }
}


combinations ( y)