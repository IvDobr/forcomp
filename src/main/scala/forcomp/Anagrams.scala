package forcomp

import common._

object Anagrams {

  /** ����� - ����� ���� `String`. */
  type Word = String

  /** ����������� - ��� ������ `List` ����. */
  type Sentence = List[Word]

  /** `Occurrences` ��� `List` ��� �������� � ������������� �����, ������� ���������� ������� ���������.
   *  ������ ������������ �� �������� ������� � ����.
   *  ��� ������� � ������ ��������.
   *
   *  ����� ������ ���, ������� � ������ ��������, �� �� ������������ **��** �������� ������� ���������
   *  
   *
   *  NB: ���� ������� ������� ����� 0, �� ���� ������ �� ������ �������������� � ������
   */
  type Occurrences = List[(Char, Int)]

  /** ������� - ��� ������ ������ ����
   *  ���� ���������������� �����-������� ��� �������� �������, `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** ������������ ����� � ������ ��������� ��������.
   *
   *  NB: ������� ��������������� ��� ����� �������� � ���������� � ������� � ������ ���������.
   *
   *  NB: �� ������� ������������ `groupBy` ��� ���������� ����� ������!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(x => x).mapValues(x => x.length).toList.sorted

  /** ������������ ����������� � ������ ��������� ��� ��������. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.flatten.mkString)
  }

  /** `dictionaryByOccurrences` ���� `Map` �� ��������� ������� ��������� � ������������������ ���� ����,
   *  ������� ����� ��� ����� ���������.
   *  ���� ��� ������������� ������� ������ ��������� ���� �������� �����.
   *
   *  ��������, ����� "eat" ����� ��������� ������ ���������:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  ��� � ����� "ate" � "tea".
   *
   *  ��� ��������, ��� `dictionaryByOccurrences` ����� ��������� ������:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(x => wordOccurrences(x))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** ���������� ������ ���� ����������� ������ ���������.
   *  �������� ���� ��������� `List(('k', 1), ('o', 1))`
   *  �������� ������������� `List(('k', 1), ('o', 1))`.
   *  ����� �������� ������ ������������ `List()`.
   *
   *  ������: ������������ ������ ��������� `List(('a', 2), ('b', 2))` ���������:
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
   *  ������� �� ����� -- ������������ � ������� ���� ����� ���� � ������ �������.
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

  /** �������� ������ ��������� `y` �� ������ ��������� `x`.
   *
   *  ����������� - �� ��� `y` �������� ������������� `x` -- ����� ������ �������� � `y`������ ������� � `x`, � ��� ������� ������ ���� ������ ���� �����
   *  ������� � `x`.
   *
   *  NB: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val (p1,p2) = x.partition(a => y.exists(b => a._1 == b._1))
    val diffed = for( (a,b) <- p1.zip(y) if a._2 != b._2) yield (a._1,a._2-b._2)
    (p2 ++ diffed).sorted
  }

  /** ���������� ������ ���� �������� �����������.
   *
   *  ��������� ����������� ����������� �� ������ ��������� ���� ��������
   *  ���� ���� ����������� � ���������� ��� ��������� ���������� ���� � ����� �������,
   *  ��� ��� ����� ��������� � �������.
   *
   *  ����� ���� � ����������� � ��� ��������� �� ������� ���������.
   *  ��������, ����������� `List("I", "love", "you")` ��������� ����������� `List("You", "olive")`.
   *
   *  �����, ����������� � ���� �� �������, ���  ������ ������� �������� ����� ���������� �����������.
   *  ��������, ����������� `List("You", "olive")` � `List("olive", "you")` ��������� ���������
   *  `List("I", "love", "you")`.
   *
   *  ����� ������ ������ ��� ����������� `List("Yes", "man")` � ��� �������� �� �������:
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
   *  ��������� ����������� �� ������� ���� � ��������� ������� - ����� ������� ��������, ���� ��� ��������� ������ � ���������.
   *  ������ ������������ ����� ������ �������������� � �������.
   *
   *  NB: � ������, ���� ����� � ����������� �� �������, �� ����������� �������� ���������� ������ ����, ������� ��� ������ �������������� � ���� ������.
   *
   *  NB: ���� ������ ���� ��������� ������� �����������.
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
