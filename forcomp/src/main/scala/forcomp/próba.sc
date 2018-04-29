val dictionar: List[Word] = forcomp.loadDictionary
val x: String = "aba"
lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
  val code = dictionar map wordOccurrences
  val pairs = dictionar zip code
  val grouped = pairs groupBy(_._2)
  grouped map { case (chars,list) => (chars, list.unzip._1) } withDefaultValue List()
}


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
  val mapa2 =mapa map { case (char, list) => (char, list.unzip._2.sum) }
  mapa2.toList  sortWith (_._1 < _._1)
//}
//sentenceOccurrences(s)
val dictionary = List("ate","tea","eat")

val code = dictionary map wordOccurrences
val pairs = dictionary zip code
val grouped = pairs groupBy(_._2)
val maped = grouped map { case (chars,list) => (chars, list.unzip._1) }

val y = List(('b',1),('c',1))

/**def combinations(occurrences: Occurrences): List[Occurrences] = {
  if (occurrences.isEmpty) List(List())
  else {
    val (a,b) = occurrences.head
    for {
      i <- (0 to b)
      sth <- combinations(occurrences.tail)
    } yield if (i > 0) (a, i) :: sth else sth
  }
}*/
def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
  case Nil => List(List())
  case (a,b) :: tail => for {
      i <- (0 to b).toList
      sth <- combinations(tail)
    } yield if (i > 0) (a, i) :: sth else sth
}

combinations ( y)

val map = y.toMap withDefaultValue 0

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val map = y.toMap withDefaultValue 0
  for {
    (a,b) <- x
    if b > map(a)
  } yield (a,b - map(a))
}

val z = List(('b',1))

def sentenceOccurrences(s: Sentence): Occurrences = {
  val list = s flatMap wordOccurrences
  val grupedSentence = list groupBy (_._1)
  val mapedSentence = grupedSentence map { case (char, lista) => (char, (lista.unzip._2).sum) }
  mapedSentence.toList  sortWith (_._1 < _._1)
}

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  val occ = sentenceOccurrences(sentence)
  def find (occ: Occurrences): List[Sentence] = {
    for {
      wordocc <- combinations(occ)
      word <- dictionaryByOccurrences(wordocc)
      rest <- find( subtract(occ,wordocc))
    //if dictionaryByOccurrences(wordocc).nonEmpty
    } yield if(occ == wordocc) List(word) else word :: rest
  }
  find(occ)
}

subtract(List(),List())

val sen = List("I","love")
sentenceAnagrams(sen)
