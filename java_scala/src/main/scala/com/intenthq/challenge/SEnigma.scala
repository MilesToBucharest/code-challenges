package com.intenthq.challenge

import scala.annotation.tailrec
;

object SEnigma {

  // We have a system to transfer information from one place to another. This system
  // involves transferring only list of digits greater than 0 (1-9). In order to decipher
  // the message encoded in the list you need to have a dictionary that will allow
  // you to do it following a set of rules:
  //    > Sample incoming message: (​1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)
  //    > Sample dictionary (​23->‘N’,234->‘ ’,89->‘H’,78->‘Q’,37 ->‘A’)
  //  - Iterating from left to right, we try to match sublists to entries of the map.
  //    A sublist is a sequence of one or more contiguous entries in the original list,
  //    eg. the sublist (1, 2) would match an entry with key 12, while the sublist (3, 2, 3)
  //    would match an entry with key 323.
  //  - Whenever a sublist matches an entry of the map, it’s replaced by the entry value.
  //    When that happens, the sublist is consumed, meaning that its elements can’t be used
  //    for another match. The elements of the mapping however, can be used as many times as needed.
  //  - If there are two possible sublist matches, starting at the same point, the longest one
  //    has priority, eg 234 would have priority over 23.
  //  - If a digit does not belong to any matching sublist, it’s output as is.
  //
  // Following the above rules, the message would be: “1N73N7 HQ”
  // Check the tests for some other (simpler) examples.

  @tailrec
  def decodeWithSingleKey(entry: (String, Char), message: String, decoded: String = ""): String = {
    val keyLength = entry._1.length
    if (message.length < keyLength) decoded + message
    else {
      val messageChunk = message.slice(0, keyLength)
      if (messageChunk == entry._1) decodeWithSingleKey(entry, message.slice(keyLength, message.length), decoded + entry._2)
      else decodeWithSingleKey(entry, message.tail, decoded + message.head)
    }
  }

  @tailrec
  def decodedCollectionBuilder(dictionary: List[(String, Char)], decodedListOfStrings: List[String]): List[String] = {
    if (dictionary.isEmpty) decodedListOfStrings
    else decodedCollectionBuilder(dictionary.tail, List(decodeWithSingleKey(dictionary.head, decodedListOfStrings.head)))
  }

  @tailrec
  def sortDictionaryForDecoding(dictionary: Seq[(String, Char)], sortedDictionary: List[(String, Char)] = List[(String, Char)]()): List[(String, Char)] = {
    val firstEntry = dictionary.head
    if (dictionary.count(_ => true) < 2) (sortedDictionary ++ List(firstEntry))
    else {
      val secondEntry = dictionary.tail.head
      if (secondEntry._1.contains(firstEntry._1) && secondEntry._1.length > firstEntry._1.length) {
        sortDictionaryForDecoding(dictionary.filter(_ != secondEntry), sortedDictionary ++ List(secondEntry))
      } else {
        sortDictionaryForDecoding(dictionary.drop(1), sortedDictionary ++ List(firstEntry))
      }
    }
  }

  def deciphe(map: Map[Int, Char])(message: List[Int]): String = {
    val trieHashToSeq: Seq[(String, Char)] = map.map(entry => (entry._1.toString, entry._2)).toSeq.sorted
    val messageAsString: String = message.mkString
    decodedCollectionBuilder(sortDictionaryForDecoding(trieHashToSeq), List(messageAsString)).mkString
  }
}