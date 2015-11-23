package forcomp

import common._

object Anagrams {

  /** Слово - всего лишь `String`. */
  type Word = String

  /** Предложение - это список `List` слов. */
  type Sentence = List[Word]

  /** `Occurrences` это `List` пар символов и положительных чисел, которые показывают частоту вхождения.
   *  Список отсортирован по алфавиту символа в паре.
   *  Все символы в нижнем регистре.
   *
   *  Любой список пар, который в нижнем регистре, но не отсортирован **НЕ** является списком вхождений
   *  
   *
   *  NB: Если частота символа равна 0, то этот символ не должен присутствовать в списке
   */
  type Occurrences = List[(Char, Int)]

  /** Словарь - это просто список слов
   *  Есть предопределенный метод-утилита для загрузки словаря, `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Конвертирует слово в список вхождений символов.
   *
   *  NB: символы рассматриваются без учета регистра и приводятся к нижнему в списке вхождений.
   *
   *  NB: вы обязаны использовать `groupBy` для реализации этого метода!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(x => x).mapValues(x => x.length).toList.sorted

  /** Конвертирует предложение в список вхождений его символов. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.flatten.mkString)
  }

  /** `dictionaryByOccurrences` есть `Map` из различных списков вхождений в последовательность всех слов,
   *  которые имеют это число вхождений.
   *  Этот мэп предоставляет удобный способ получения всех анаграмм слова.
   *
   *  Например, слово "eat" имеет следующий список вхождений:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Как и слова "ate" и "tea".
   *
   *  Это означает, что `dictionaryByOccurrences` будет содержать запись:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(x => wordOccurrences(x))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Возвращает список всех подмножеств списка вхождений.
   *  Включает само вхождение `List(('k', 1), ('o', 1))`
   *  является подмножеством `List(('k', 1), ('o', 1))`.
   *  Также включает пустое подмножество `List()`.
   *
   *  Пример: подмножества списка вхождений `List(('a', 2), ('b', 2))` следующие:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Порядок не важен -- подмножества в примере выше могут идти в другом порядке.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case (_,0) :: ys => combinations(ys)
    case (c,n) :: ys => combinations((c,n-1) :: ys) ++ combinations((c,n-1) :: ys).map(zs =>
      zs.find(_._1 == c) match {
        case Some(_) => zs.map(_ match {
          case (d, o) => if (d == c) (d,o+1) else (d, o)
        })
        case None => (c,1) :: zs
      })
  }

  /** Вычитает список вхождения `y` из списка вхождений `x`.
   *
   *  Предусловие - то что `y` является подмножеством `x` -- любой символ входящий в `y`должен входить в `x`, и его частота должна быть меньше либо равна
   *  частоте в `x`.
   *
   *  NB: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val (p1,p2) = x.partition(a => y.exists(b => a._1 == b._1))
    val diffed = for( (a,b) <- p1.zip(y) if a._2 != b._2) yield (a._1,a._2-b._2)
    (p2 ++ diffed).sorted
  }

  /** Возвращает список всех анаграмм предложения.
   *
   *  Анаграмма предложения формируется по списку вхождений всех символов
   *  всех слов предложения и возвращает все возможные комибнации слов с этими буквами,
   *  так что слова находятся в словаре.
   *
   *  Число слов в предложении и его анаграммы не обязаны совпадать.
   *  Например, предложение `List("I", "love", "you")` анаграмма предложения `List("You", "olive")`.
   *
   *  Также, предложения с теми же словами, нов  другом порядке являются двумя различными анаграммами.
   *  Например, предложения `List("You", "olive")` и `List("olive", "you")` различные анаграммы
   *  `List("I", "love", "you")`.
   *
   *  Здесь полный пример для предложения `List("Yes", "man")` и его анаграмм из словаря:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  Различные предложения не обязаны быть в указанном порядке - любой порядок подходит, если все анаграммы входят в множество.
   *  Каждое возвращаемое слово должно присутствовать в словаре.
   *
   *  NB: в случае, если слова в предложении из словаря, то предложение является анаграммой самого себя, поэтому оно должно присутствовать в этом списке.
   *
   *  NB: Есть только одна анаграмма пустого предложения.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    sentenceAnagrams0(sentenceOccurrences(sentence))
  }

  def sentenceAnagrams0(o: Occurrences): List[Sentence] = {
    if (o.isEmpty) {
      List(Nil)
    } else {
      val combs = combinations(o)
      for (i <- combs if dictionaryByOccurrences.keySet(i);
           j <- dictionaryByOccurrences(i);
           s <- sentenceAnagrams0(subtract(o, i))) yield {j :: s}
    }
  }

  def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] = {
    sentenceAnagrams0(sentenceOccurrences(sentence))

    //
    //
    //
    //
  }
}
