package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("singleton") {
    new TestTrees {
      assert(singleton(List()) === false)
      assert(singleton(List(t1, t2)) === false)
      assert(singleton(List(t2)) === true)
    }
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    assert(combine(leaflist) ===
      List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))

    assert(combine(List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('a', 6))) ===
      List(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), Leaf('a', 6)))

    assert(combine(List(Leaf('e', 1))) === List(Leaf('e', 1)))
  }

  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) ===
      List(makeCodeTree(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))))
  }

  test("createCodeTree") {
    assert(createCodeTree(string2Chars("xtxextx")) ===
      makeCodeTree(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === List('a', 'b'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    val codeTable = List(('a', List(0)), ('b', List(1, 0)), ('c', List(1, 1)))
    assert(codeBits(codeTable)('c') === List(1, 1))
  }

  //  test("convert"){
  //    val codeTable = List(('a', List(0)), ('b', List(1, 0)), ('c', List(1, 1)))
  //    assert(convert(createCodeTree(string2Chars("aaabbc"))) === codeTable)
  //  }

  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(t1)(string2Chars("ab")) === List(0, 1))
      assert(quickEncode(t2)(string2Chars("ab")) === List(0, 0, 0, 1))
    }
  }
}
