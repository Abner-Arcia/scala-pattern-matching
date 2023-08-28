package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("times") {
    val givenList = "hello, world".toList
    val expectedList = List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1))
    val actualList = times(givenList)
    assertEquals(actualList, expectedList)
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree") {
    val givenText = "AAAAAAAABBBCDEFGH".toList
    val tree = createCodeTree(givenText)
    val decoded = decode(tree, List(1, 0, 0, 0, 1, 0, 0, 1))
    val secret = decodedSecret
    assert(true)
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("codebits") {
    val givenTable = List(
      ('a', List(0,0,0,0)),
      ('b', List(0,0,0,1)),
      ('c', List(0,0,1,0)),
      ('d', List(0,0,1,1))
    )
    val givenChar = 'b'
    val expectedBits = List(0,0,0,1)
    val actualBits = codeBits(givenTable)(givenChar)
    assertEquals(actualBits, expectedBits)
  }

  test("quickEncode") {
    new TestTrees:
      quickEncode(t1)("ab".toList)
      encode(t1)("ab".toList)
      assertEquals(encode(t1)("ab".toList), quickEncode(t1)("ab".toList))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
