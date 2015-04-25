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
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val leaflist1 = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
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

  test("times") {
    new TestTrees {
      assert(times(List('a', 'b', 'a', 'c', 'b', 'a')) === List(('a', 3), ('b', 2), ('c', 1)))
      assert(times(List('d', 'd', 'd')) === List(('d', 3)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    new TestTrees {
      assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
      assert(combine(leaflist1) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
    }
  }

  test("until one tree left") {
    new TestTrees {
      assert(until(singleton, combine)(leaflist) === List(
        Fork(
          Fork(
            Leaf('e',1),Leaf('t',2),List('e', 't'),3
          ),
          Leaf('x', 4),List('e', 't', 'x'),7
        )
      ))
      assert(until(singleton, combine)(Nil) === Nil)
    }
  }

  test("decodeSecret") {
    assert(decodedSecret.mkString === "huffmanestcool")
  }

  test("encode") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(encode(frenchCode)("huffmanestcool".toList).equals(secret))
    }
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
      assert(quickEncode(frenchCode)("huffmanestcool".toList).equals(secret))
    }
  }
}
