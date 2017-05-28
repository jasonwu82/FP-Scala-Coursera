package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  trait TestTrees2 {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)

  }
  test("decode and encode a very short text should be identity") {
    new TestTrees2 {
      println("t1 is...")
      println(encode(t1)("ab".toList))
      println((decode(t1, encode(t1)("ab".toList)).toString))

      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)

    }
  }

  trait TestList {
    val chars = List('a','b','a')
    val a_leaf = Leaf('a',2)
    val b_leaf = Leaf('b',1)
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

  }
  test("times"){
    new TestList {

      assert(times(chars) === List(('b',1),('a',2)))
    }
  }
  test("combine"){
    new TestList {

      assert(combine(List(a_leaf,b_leaf)) === List(Fork(a_leaf,b_leaf,List('a','b'),3)))
      //
    }

  }
  test("create"){
    new TestList {
      println(createCodeTree(chars))
    }
  }
  /*trait TestList2 {
    val chars = string2Chars("ettxxxx")

  }
  test("orderList"){
    new TestList2 {
      val char_time = times(chars)
      println(char_time)
      val sorted_leaf = makeOrderedLeafList(char_time)
      println(sorted_leaf)
      info(sorted_leaf.toString)
    }
  }
  test("decode"){
    new TestList2 {
      println(chars)
      //val x = createCodeTree(chars)
      //println(x)
    }
  }*/
}
class HuffmanSuite2 extends FunSuite {
  trait TestList2 {
    val chars = string2Chars("ettxxxx")
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }
  test("orderList"){
    new TestList2 {
      val char_time = times(chars)

      val sorted_leaf = makeOrderedLeafList(char_time)

      info(sorted_leaf.toString)
    }
  }
  test("decode"){
    new TestList2 {
      println(chars)
      val x = createCodeTree(chars)
      println(x)
    }
  }
  test("decode simple"){
    new TestList2 {
      val simple = decode(t2,List(0,0,1,0,1) )
      println(simple)
    }

  }
  test("decode frenchCode"){
    val french_decoded = decode(frenchCode,secret)
    println(french_decoded)
  }
}

class myHuffmanSuite3 extends FunSuite{
  trait TestTree {

    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }
  test("convert"){
    new TestTree {
      println(convert(t2))
    }

  }
  trait TestTrees2 {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
  }
  test("decode and encode a very short text should be identity") {
    new TestTrees2 {

      assert(encode(t1)("ab".toList) === quickEncode(t1)("ab".toList))

    }
  }
}