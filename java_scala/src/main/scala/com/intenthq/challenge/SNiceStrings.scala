package com.intenthq.challenge

import scala.annotation.tailrec

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  @tailrec
  def vowelHelper(subject: String, vowelsReq: Int = 3, accum: Int = 0): Boolean = {
    val vowels: Array[Char] = Array('a', 'e', 'i', 'o', 'u')
    if (vowelsReq == accum) true
    else if (subject.length < vowelsReq - accum) false
    else if (vowels.contains(subject.head)) vowelHelper(subject.tail, vowelsReq, accum + 1)
    else vowelHelper(subject.tail, vowelsReq, accum)
  }

  @tailrec
  def seqLetterHelper(subject: String, seqLetterReq: Int = 2): Boolean = {
    if (subject.length < seqLetterReq) false
    else if (subject.head == subject.tail.head) true
    else seqLetterHelper(subject.tail)
  }

  def comboHelper(subject: String): Boolean = {
    val bannedCombos: Array[String] = Array("ab", "cd", "pq", "xy")
    !bannedCombos.exists(bc => subject.contains(bc))
  }

  def nice(xs: List[String]): Int = {
    xs.count(x => vowelHelper(x) && seqLetterHelper(x) && comboHelper(x))
  }
}
