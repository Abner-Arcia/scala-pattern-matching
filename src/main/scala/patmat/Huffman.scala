package patmat

import scala.annotation.tailrec

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface:

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, chars, weight) => weight
    case Leaf(char, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, chars, weight) => chars
    case Leaf(char, weight) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree): Fork =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {

    /**
     * The purpose of this function is to pass it a character and a list of pairs (char, count) and get an updated list
     * where if there was an existing pair with that character its count is increased and if there wasn't then a new
     * pair is appended with that character and an initial count of 1
     *
     * The base cases are:
     *  If you finished traversing the list and didn't find a pair with the char, then return a list with a new pair
     *  If you found a pair with the char then increment the count and prepend the pair to the tail of the list
     *
     * If the current pair isn't a match then we need to check the next pair, and to do that while preserving this pair
     * in the list we prepend it with a recursive call
     */
    def increaseCharCount(char: Char, pairs: List[(Char, Int)]): List[(Char, Int)] = {
      if pairs.isEmpty then return List((char, 1))
      val currentPair = pairs.head
      currentPair match {
        case (currentChar, count) =>
          if currentChar == char then return (char, count + 1) :: pairs.tail
      }
      currentPair :: increaseCharCount(char, pairs.tail)
    }

    /**
     * Traverse the list and for each char increase its count
     *
     * In each iteration the grab the current char and the count, and we get an updated count, and the rest of the list
     * and the updated count is passed to the next iteration
     * When the list is empty simply return the final count
     */
    @tailrec
    def timesAcc(chars: List[Char], pairs: List[(Char, Int)]): List[(Char, Int)] = {
      if chars.isEmpty then return pairs
      val foundPairs = increaseCharCount(chars.head, pairs)
      timesAcc(chars.tail, foundPairs)
    }

    timesAcc(chars, List())
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    freqs.map((char, int) => Leaf(char, int)).sortWith((leaf1, leaf2) => leaf1.weight < leaf2.weight)

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if singleton(trees) then return trees
    val firstTree = trees.head
    val secondTree = trees.tail.head
    val listWithTreesRemoved = trees.filter(tree => chars(tree) != chars(firstTree) && chars(tree) != chars(secondTree))
    val newTree = makeCodeTree(firstTree, secondTree)
    val unsortedListWithNewTree = newTree :: listWithTreesRemoved
    unsortedListWithNewTree.sortWith((tree1, tree2) => weight(tree1) < weight(tree2))
  }

  /**
   * For understanding this function we just need to keep in mind that done refers to the singleton function, merge
   * refers to combine and trees initially refers to the leaves but as they are combined it will refer to a mix of forks
   * and leaves until we get a single root fork. Basically, combine the trees until we get a single root
   */
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if done(trees) then return trees
    val newTrees = merge(trees)
    until(done, merge)(newTrees)
  }

  /**
   * Put together the functions defined above to create a code tree
   */
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val pairs = times(chars)
    val leafList = makeOrderedLeafList(pairs)
    val singletonList = until(singleton, combine)(leafList)
    singletonList.head
  }


  // Part 3: Decoding

  type Bit = Int

  /**
   * The general procedure to decode a message is to start reading the list of bits, and if the bit is 0 then we take
   * to the left and if it's 1 we take to the right. Once we get to a leaf node we append the character it represents
   * to our acc list. We stop when all the bits have been processed
   *
   * In more detail, if the current tree is a fork, the current bit is the head of the bit list. If this bit is 0 then
   * for the next recursive call we'll pass the left sub tree and if it's 1 the next tree will be the right sub tree.
   * We also pass the remaining bits and the char acc. When the bit list is empty we return the acc which should be the
   * decoded text
   *
   * If the current tree is a leaf then we get its char and append it to the acc, and we search for the next char from
   * the root of the tree (the initial tree)
   */
  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    @tailrec
    def decodeAcc(newTree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = newTree match {
      case Leaf(char, weight) =>
        val newAcc = acc :+ char
        decodeAcc(tree, bits, newAcc)
      case Fork(left, right, chars, weight) =>
        if bits.isEmpty then return acc
        val currentBit = bits.head
        val nextTree = if currentBit == 0 then left else right
        decodeAcc(nextTree, bits.tail, acc)
    }
    decodeAcc(tree, bits, List())
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * The process is really similar to a decoding, but instead of traversing the bits we'll traverse the chars.
   * For a given char, we need to check in which sub tree does the char appear, and go to that one, appending the
   * corresponding bit to our acc of bits. Once we get to a leaf node we move on to the next char and start from the
   * root again. When chars are over we return the acc which should be the encoded text
   *
   * The condition of the text being empty should come first because whether the current tree is a leaf or fork we
   * perform an operation on the list and that's illegal on an empty list
   *
   * If the current tree is a fork, we take the current char and check whether it is contained in the left sub tree or
   * not. Based on this, the next tree to check will be the left one and we'll append 0 to the acc or will be the right
   * one and we'll append 1. We go to the next iteration with our next tree and updated acc
   *
   * If the current tree is a leaf then we start from the root again, advance to the next char and keep the current acc
   */
  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {

    @tailrec
    def encodeAcc(newTree: CodeTree, text: List[Char], acc: List[Bit]): List[Bit] = {
      if text.isEmpty then return acc
      newTree match {
        case Leaf(char, weight) => encodeAcc(tree, text.tail, acc)
        case Fork(left, right, chars1, weight) =>
          val currentChar = text.head
          val shouldTakeLeft = chars(left) contains currentChar
          val (nextTree, bit) = if shouldTakeLeft then (left, 0) else (right, 1)
          val newAcc = acc :+ bit
          encodeAcc(nextTree, text, newAcc)
      }
    }

    encodeAcc(tree, text, List())
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.filter(pair => pair._1 == char).head._2

  /**
   * Treating each sub tree as a code table, we can get a bigger code table by merging them both. If we apply this
   * process recursively then we'll get the general code table. Basically, we are traversing the entire tree
   * and accumulating the bits until we reach a leaf, then we return the char in the leaf and its final bits, which is
   * considered a sub code table, with only one encoding. We do this for each sub tree and merge the resulting sub code
   * tables until we have traversed the whole tree
   *
   * Starting at the root, to get the code table, we need to merge the code table represented by the left sub tree with
   * the one represented by the right sub tree. Once we get them both we simply apply a merge. Of course this is
   * recursive and to get those code tables we need to apply this same process to their trees, and for getting those to
   * the left, we'll append 0 to the acc, and append 1 for those on the right
   *
   * Once we reach a leaf node, we'll return a list with a single pair of the char in the leaf and the acc gotten for it
   * With this list we can do the merging all the way up to the root
   */
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
    def convert(tree: CodeTree): CodeTable = {

      def convertAcc(tree: CodeTree, acc: List[Bit]): CodeTable = tree match {
        case Leaf(char, weight) => List((char, acc))
        case Fork(left, right, chars, weight) =>
          val leftTable = convertAcc(left, acc :+ 0)
          val rightTable = convertAcc(right, acc :+ 1)
          mergeCodeTables(leftTable, rightTable)
      }

      convertAcc(tree, List())
    }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  /**
   * We convert the code tree to a code table, and then we map each char in the text to its encoded representation,
   * which is a list of bits. This results in a list that contains list of bits, so we need to remove the unnecessary
   * nesting to simply get a list of bits, and for that we apply flatten
   */
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)
    val listOfListOfBits = text.map(char => codeBits(codeTable)(char))
    listOfListOfBits.flatten
  }

object Huffman extends Huffman
