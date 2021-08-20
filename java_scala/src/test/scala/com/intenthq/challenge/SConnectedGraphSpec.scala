package com.intenthq.challenge

import org.specs2.mutable.Specification

class SConnectedGraphSpec extends Specification {

  section("scala")
  section("graph")
  "SConnectedGraph" should {
    "work with an acyclic graph" in {
      val dest = Node(9)
      val n10 = Node(10)
      val n8 = Node(8, List(dest))
      val start = Node(3, List(n8, n10))
      val start2 = Node(11, List(Node(2), dest, n10))
      val start3 = Node(5,List(start2))
      SConnectedGraph.run(start, dest) must_== true
      SConnectedGraph.run(start2, dest) must_== true
      SConnectedGraph.run(start, dest) must_== true
      SConnectedGraph.run(start2, dest) must_== true
      SConnectedGraph.run(start3, dest) must_== true
      SConnectedGraph.run(dest, start3) must_== false
      SConnectedGraph.run(dest, start2) must_== false
      SConnectedGraph.run(start,start2) must_==  false
    }
    "a node is connected to itself" in {
      val start = Node(1)
      SConnectedGraph.run(start, start) must_== true
    }
    "work with a larger acyclic graph" in {
      val n9 = Node(9)
      val n8 = Node(8, List(n9))
      val n10 = Node(10)
      val n3 = Node(3, List(n8, n10))
      val n2 = Node(2)
      val n11 = Node(11, List(n2, n9, n10))
      val n7 = Node(7, List(n11, n8))
      val n5 = Node(5, List(n11))
      SConnectedGraph.run(n7, n10) must_== true
      SConnectedGraph.run(n5, n2) must_== true
      SConnectedGraph.run(n3, n9) must_== true
      SConnectedGraph.run(n7, n10) must_== true
      SConnectedGraph.run(n5, n8) must_== false
      SConnectedGraph.run(n2, n3) must_== false
      SConnectedGraph.run(n8, n10) must_== false
    }
  }
  section("graph")
  section("scala")
}