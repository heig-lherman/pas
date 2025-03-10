package labTestSuites.testSuite.herman_holzer_stefani

import labTestSuites.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}

import scala.annotation.tailrec

/** Référence all the implementations so that they can be used in the automated
  * tests. SHOULD NOT BE MODIFIED !!!
  */
val all = Seq(
  classOf[XSuite],
  classOf[B1Suite],
  classOf[B2Suite],
  classOf[B3Suite],
  classOf[B4Suite],
  classOf[C1Suite],
  classOf[C2Suite],
  classOf[C3Suite],
  classOf[C4Suite],
  classOf[C5Suite],
  classOf[C6Suite],
  classOf[C7Suite],
  classOf[C8Suite],
  classOf[C9Suite],
  classOf[C10Suite],
  classOf[D1Suite],
  classOf[D2Suite],
  classOf[D3Suite],
  classOf[D4Suite],
  classOf[D5Suite],
  classOf[D6Suite],
  classOf[D7Suite],
  classOf[D8Suite],
  classOf[D9Suite],
  classOf[D10Suite],
  classOf[D11Suite],
  classOf[D12Suite],
  classOf[D13Suite],
  classOf[D14Suite],
  classOf[D15Suite],
  classOf[D16Suite],
)

class XSuite extends ScalaCheckSuite {

  /** From all the implementation retrieve only the first one about the current
    * exercice.
    *
    * Using collect allows us to do a filter while specializing the type of the
    * collection.
    */
  val impl: X = getImplementations().collect { case x: X => x }.head

  /** A simple test that will succeed.
    */
  test("2 to the power of 2 should be 4") {
    assertEquals(impl.power(2, 2), 4.0)
  }

  /** A simple test that will fail (because the implementation is not general
    * enough).
    *
    * In your submission, ideally none of your tests should fail for your own
    * implementation.
    */
  test("2 to the power of -2 should be 0.25".ignore) {
    assertEquals(impl.power(2, -2), 0.25)
  }

  /** A test (using scalacheck) that will check that a random selection of
    * double value to the power of 0 is equal to 1.
    *
    * The random selection of value is decided at runtime.
    */
  property("any value to the power of 0 should be 1") {
    forAll { (x: Double) =>
      assertEquals(impl.power(x, 0), 1.0)
    }
  }
}

class B1Suite extends ScalaCheckSuite {
  val impl: B1 = getImplementations().collect { case x: B1 => x }.head

  test("B1: sum of empty list should be 0") {
    assertEquals(impl.func(List.empty[Int]), 0)
  }

  test("B1: sum of single element list") {
    assertEquals(impl.func(List(5)), 5)
  }

  test("B1: sum of multiple elements") {
    assertEquals(impl.func(List(1, 2, 3, 4)), 10)
  }

  test("B1: sum of list with negative numbers") {
    assertEquals(impl.func(List(-1, -2, -3)), -6)
  }

  test("B1: sum of mixed positive and negative numbers") {
    assertEquals(impl.func(List(-1, 2, -3, 4)), 2)
  }

  property("B1: sum of list elements should be correct") {
    forAll { (xs: List[Int]) =>
      assertEquals(impl.func(xs), xs.sum)
    }
  }
}

class B2Suite extends ScalaCheckSuite {
  val impl: B2 = getImplementations().collect { case x: B2 => x }.head

  test("B2: vector contains numbers from 1 to 1000") {
    val vector = impl.vector
    assertEquals(vector.length, 1000)
    assertEquals(vector.head, 1)
    assertEquals(vector.last, 1000)
  }

  test("B2: func returns cube for multiples of 3") {
    val multiplesOf3 = Seq(3, 6, 9, 12, 15, 21, 27, 99, 300, 999)
    multiplesOf3.foreach { n =>
      assertEquals(impl.func(n), n * n * n, s"Failed for $n")
    }
  }

  test("B2: func returns square for even numbers not divisible by 3") {
    val evenNotMultipleOf3 = Seq(2, 4, 8, 10, 14, 16, 20, 22, 50, 100, 998)
    evenNotMultipleOf3.foreach { n =>
      assertEquals(impl.func(n), n * n, s"Failed for $n")
    }
  }

  test("B2: func returns -1 for odd numbers not divisible by 3") {
    val oddNotMultipleOf3 = Seq(1, 5, 7, 11, 13, 17, 19, 23, 25, 37, 101, 997)
    oddNotMultipleOf3.foreach { n =>
      assertEquals(impl.func(n), -1, s"Failed for $n")
    }
  }

  property("B2: func returns cube for all multiples of 3") {
    val multiplesOf3Gen = Gen.choose(1, 333).map(_ * 3)

    forAll(multiplesOf3Gen) { n =>
      impl.func(n) == n * n * n
    }
  }

  property("B2: func returns square for all even numbers not divisible by 3") {
    val evenNotMultipleOf3Gen =
      Gen.choose(1, 500).map(_ * 2).suchThat(_ % 3 != 0)

    forAll(evenNotMultipleOf3Gen) { n =>
      impl.func(n) == n * n
    }
  }

  property("B2: func returns -1 for all odd numbers not divisible by 3") {
    val oddNotMultipleOf3Gen =
      Gen.choose(0, 499).map(n => 2 * n + 1).suchThat(_ % 3 != 0)

    forAll(oddNotMultipleOf3Gen) { n =>
      impl.func(n) == -1
    }
  }
}

class B3Suite extends ScalaCheckSuite {
  val impl: B3 = getImplementations().collect { case x: B3 => x }.head

  test("B3: factorial of 0 should be 1") {
    assertEquals(impl.fact(0), BigInt(1))
  }

  test("B3: factorial of 1 should be 1") {
    assertEquals(impl.fact(1), BigInt(1))
  }

  test("B3: factorial of 5 should be correct") {
    assertEquals(impl.fact(5), BigInt(120))
  }

  test("B3: fibonacci of negative number should be -1") {
    assertEquals(impl.fib(-1), BigInt(-1))
  }

  test("B3: fibonacci of 0 should be 0") {
    assertEquals(impl.fib(0), BigInt(0))
  }

  test("B3: fibonacci of 1 should be 1") {
    assertEquals(impl.fib(1), BigInt(1))
  }

  test("B3: fibonacci of 5 should be correct") {
    assertEquals(impl.fib(5), BigInt(5))
  }

  test("B3: tail recursive fibonacci of 0 should be 0") {
    assertEquals(impl.fibRec(0), BigInt(0))
  }

  test("B3: tail recursive fibonacci of 1 should be 1") {
    assertEquals(impl.fibRec(1), BigInt(1))
  }

  test("B3: tail recursive fibonacci of 5 should be correct") {
    assertEquals(impl.fibRec(5), BigInt(5))
  }

  test("B3: apply vector should have correct length") {
    assertEquals(impl.apply.length, 10)
  }

  property("B3: factorial(n+1) = (n+1) * factorial(n)") {
    val genSmallInt = Gen.choose(0, 10)

    forAll(genSmallInt) { n =>
      impl.fact(n + 1) == (n + 1) * impl.fact(n)
    }
  }

  property("B3: fibonacci(n) = fibonacci(n-1) + fibonacci(n-2) for n > 1") {
    val genFibIndex = Gen.choose(2, 10)

    forAll(genFibIndex) { n =>
      impl.fib(n) == impl.fib(n - 1) + impl.fib(n - 2)
    }
  }

  property("B3: both fibonacci implementations produce the same results") {
    val genFibIndex = Gen.choose(0, 20)

    forAll(genFibIndex) { n =>
      impl.fib(n) == impl.fibRec(n)
    }
  }
}

class B4Suite extends ScalaCheckSuite {
  val impl: B4 = getImplementations().collect { case x: B4 => x }.head

  test("B4: any number to the power of 0 should be 1") {
    assertEquals(impl.fastExp(5, 0), 1)
    assertEquals(impl.fastExp(10, 0), 1)
    assertEquals(impl.fastExp(0, 0), 1)
  }

  test("B4: negative exponent should return -1") {
    assertEquals(impl.fastExp(2, -1), -1)
    assertEquals(impl.fastExp(5, -3), -1)
  }

  test("B4: positive even exponent should work correctly") {
    assertEquals(impl.fastExp(2, 4), 16)
    assertEquals(impl.fastExp(3, 6), 729)
  }

  test("B4: positive odd exponent should work correctly") {
    assertEquals(impl.fastExp(2, 3), 8)
    assertEquals(impl.fastExp(3, 5), 243)
  }

  property(
    "B4: fast exponentiation should match Math.pow for non-negative inputs",
  ) {
    forAll(Gen.choose(0, 10), Gen.choose(0, 6)) { (base: Int, exp: Int) =>
      val expected = math.pow(base, exp).toInt
      impl.fastExp(base, exp) == expected
    }
  }

  test("B4: base 0 with positive exponent") {
    assertEquals(impl.fastExp(0, 5), 0)
  }

  test("B4: base 1 with any exponent") {
    assertEquals(impl.fastExp(1, 100), 1)
  }
}

class C1Suite extends ScalaCheckSuite {
  val impl: C1 = getImplementations().collect { case x: C1 => x }.head

  test("C1: zip empty vectors and lists") {
    assertEquals(impl.zip(Vector(), List()), List())
  }

  test("C1: zip with empty vector") {
    assertEquals(impl.zip(Vector(), List(1, 2, 3)), List())
  }

  test("C1: zip with empty list") {
    assertEquals(impl.zip(Vector("a", "b", "c"), List()), List())
  }

  test("C1: zip with equal length") {
    assertEquals(
      impl.zip(Vector("a", "b", "c"), List(1, 2, 3)),
      List((1, "a"), (2, "b"), (3, "c")),
    )
  }

  test("C1: zip with vector longer than list") {
    assertEquals(
      impl.zip(Vector("a", "b", "c", "d"), List(1, 2)),
      List((1, "a"), (2, "b")),
    )
  }

  test("C1: zip with list longer than vector") {
    assertEquals(
      impl.zip(Vector("a", "b"), List(1, 2, 3, 4)),
      List((1, "a"), (2, "b")),
    )
  }

  test("C1: toMap with unique keys") {
    val input = List((1, "a"), (2, "b"), (3, "c"))
    val expected = Map(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(impl.toMap(input), expected)
  }

  test("C1: toMap with duplicate keys") {
    val input = List((1, "a"), (2, "b"), (1, "c"))
    val expected = Map(1 -> "c", 2 -> "b")
    assertEquals(impl.toMap(input), expected)
  }

  property("C1: combined length equals min of input lengths") {
    val genVector = Gen.listOf(Gen.alphaNumStr).map(Vector(_: _*))
    val genList = Gen.listOf(Gen.choose(1, 100))

    forAll(genVector, genList) { (vec, list) =>
      val pairs = impl.zip(vec, list)
      pairs.length == Math.min(vec.length, list.length)
    }
  }

  property("C1: keys in result map are from the input list") {
    val genVector = Gen.listOf(Gen.alphaNumStr).map(Vector(_: _*))
    val genList = Gen
      .listOf(Gen.choose(1, 100))
      .map(_.distinct)

    forAll(genVector, genList) { (vec, list) =>
      val pairs = impl.zip(vec, list)
      val map = impl.toMap(pairs)
      val minLength = Math.min(vec.length, list.length)
      val expectedKeys = list.take(minLength).toSet

      map.keySet == expectedKeys
    }
  }

  property("C1: values in result map are from the input vector") {
    val genVector = Gen.listOf(Gen.alphaNumStr).map(Vector(_: _*))
    val genList = Gen
      .listOf(Gen.choose(1, 100))
      .map(_.distinct)

    forAll(genVector, genList) { (vec, list) =>
      val pairs = impl.zip(vec, list)
      val map = impl.toMap(pairs)
      val minLength = Math.min(vec.length, list.length)
      val expectedValues = vec.take(minLength).toSet

      map.values.toSet == expectedValues
    }
  }
}

class C2Suite extends ScalaCheckSuite {
  val impl: C2 = getImplementations().collect { case x: C2 => x }.head

  test("C2: init of empty list throws exception") {
    intercept[Throwable] {
      impl.init(List())
    }
  }

  test("C2: init of single element list") {
    assertEquals(impl.init(List(1)), List())
  }

  test("C2: init of multiple element list") {
    assertEquals(impl.init(List(1, 2, 3, 4)), List(1, 2, 3))
  }

  property("C2: init returns all but the last element") {
    forAll { (xs: List[Int]) =>
      xs.nonEmpty ==> {
        impl.init(xs) == xs.dropRight(1)
      }
    }
  }
}

class C3Suite extends ScalaCheckSuite {
  val impl: C3 = getImplementations().collect { case x: C3 => x }.head

  test("C3: penultimate of empty list throws IllegalArgumentException") {
    intercept[IllegalArgumentException] {
      impl.penultimate(List())
    }
  }

  test(
    "C3: penultimate of single element list throws IllegalArgumentException",
  ) {
    intercept[IllegalArgumentException] {
      impl.penultimate(List(1))
    }
  }

  test("C3: penultimate of two-element list") {
    assertEquals(impl.penultimate(List(1, 2)), 1)
  }

  test("C3: penultimate of multiple element list") {
    assertEquals(impl.penultimate(List(1, 2, 3, 4)), 3)
  }

  property("C3: penultimate returns second-to-last element") {
    forAll { (xs: List[Int]) =>
      (xs.length >= 2) ==> {
        impl.penultimate(xs) == xs(xs.length - 2)
      }
    }
  }
}

class C4Suite extends ScalaCheckSuite {
  val impl: C4 = getImplementations().collect { case x: C4 => x }.head

  test("C4: isPalindrome of empty list") {
    assert(impl.isPalindrome(List()))
  }

  test("C4: isPalindrome of single element list") {
    assert(impl.isPalindrome(List(1)))
  }

  test("C4: isPalindrome of palindrome") {
    assert(impl.isPalindrome(List(1, 2, 3, 2, 1)))
  }

  test("C4: isPalindrome of non-palindrome") {
    assert(!impl.isPalindrome(List(1, 2, 3, 4)))
  }

  property("C4: a list concatenated with its reverse is always a palindrome") {
    forAll { (list: List[Int]) =>
      val palindrome = list ++ list.reverse
      impl.isPalindrome(palindrome)
    }
  }

  property(
    "C4: a list with identical first and last element and palindrome middle is a palindrome",
  ) {
    forAll { (element: Int, list: List[Int]) =>
      val palindromeMiddle = list ++ list.reverse
      val palindrome = (element :: palindromeMiddle) :+ element
      impl.isPalindrome(palindrome)
    }
  }

  property("C4: reverse of a palindrome is still a palindrome") {
    val genPalindrome = for {
      list <- Gen.listOf(Gen.choose(1, 100))
      palindrome = list ++ list.reverse
    } yield palindrome

    forAll(genPalindrome) { palindrome =>
      impl.isPalindrome(palindrome.reverse) == impl.isPalindrome(palindrome)
    }
  }

  property(
    "C4: removing the middle element from an odd-length palindrome preserves the palindrome property",
  ) {
    val genOddPalindrome = for {
      list <- Gen.nonEmptyListOf(Gen.choose(1, 100))
      middle <- Gen.choose(1, 100)
      palindrome = (list :+ middle) ++ list.reverse
    } yield palindrome

    forAll(genOddPalindrome) { palindrome =>
      val middle = palindrome.length / 2
      val withoutMiddle = palindrome.take(middle) ++ palindrome.drop(middle + 1)
      impl.isPalindrome(withoutMiddle)
    }
  }

  property("C4: a list is a palindrome if and only if it equals its reverse") {
    forAll { (list: List[String]) =>
      impl.isPalindrome(list) == (list == list.reverse)
    }
  }
}

class C5Suite extends ScalaCheckSuite {
  val impl: C5 = getImplementations().collect { case x: C5 => x }.head

  test("C5: removeAt from empty list") {
    assertEquals(impl.removeAt(0, List()), List())
  }

  test("C5: removeAt with index out of bounds") {
    assertEquals(impl.removeAt(5, List(1, 2, 3)), List(1, 2, 3))
  }

  test("C5: removeAt first element") {
    assertEquals(impl.removeAt(0, List(1, 2, 3)), List(2, 3))
  }

  test("C5: removeAt middle element") {
    assertEquals(impl.removeAt(1, List(1, 2, 3)), List(1, 3))
  }

  test("C5: removeAt last element") {
    assertEquals(impl.removeAt(2, List(1, 2, 3)), List(1, 2))
  }

  property("C5: removeAt removes only the specified element") {
    forAll { (xs: List[Int], n: Int) =>
      if (xs.isEmpty || n < 0 || n >= xs.length) {
        impl.removeAt(n, xs) == xs
      } else {
        val (before, after) = xs.splitAt(n)
        impl.removeAt(n, xs) == before ::: after.tail
      }
    }
  }
}

class C6Suite extends ScalaCheckSuite {
  val impl: C6 = getImplementations().collect { case x: C6 => x }.head

  test("C6: pack of empty list") {
    assertEquals(impl.pack(List()), List())
  }

  test("C6: pack of single element list") {
    assertEquals(impl.pack(List(1)), List(List(1)))
  }

  test("C6: pack of list with consecutive duplicates") {
    assertEquals(
      impl.pack(List(1, 1, 2, 2, 2, 3, 4, 4)),
      List(List(1, 1), List(2, 2, 2), List(3), List(4, 4)),
    )
  }

  test("C6: pack of list without duplicates") {
    assertEquals(
      impl.pack(List(1, 2, 3, 4)),
      List(List(1), List(2), List(3), List(4)),
    )
  }

  property("C6: pack groups consecutive equal elements") {
    forAll { (xs: List[Int]) =>
      if (xs.isEmpty) {
        impl.pack(xs) == List()
      } else {
        impl.pack(xs).forall(group => group.forall(_ == group.head)) &&
        impl.pack(xs).flatten == xs
      }
    }
  }
}

class C7Suite extends ScalaCheckSuite {
  val impl: C7 = getImplementations().collect { case x: C7 => x }.head

  test("C7: encode of empty list") {
    assertEquals(impl.encode(List()), List())
  }

  test("C7: encode of single element list") {
    assertEquals(impl.encode(List(1)), List((1, 1)))
  }

  test("C7: encode of list with consecutive duplicates") {
    assertEquals(
      impl.encode(List(1, 1, 2, 2, 2, 3, 4, 4)),
      List((1, 2), (2, 3), (3, 1), (4, 2)),
    )
  }

  test("C7: encode of list without duplicates") {
    assertEquals(
      impl.encode(List(1, 2, 3, 4)),
      List((1, 1), (2, 1), (3, 1), (4, 1)),
    )
  }

  property("C7: encode counts consecutive occurrences correctly") {
    forAll { (xs: List[Int]) =>
      if (xs.isEmpty) {
        impl.encode(xs) == List()
      } else {
        val encoded = impl.encode(xs)
        encoded.flatMap { case (e, n) => List.fill(n)(e) } == xs
      }
    }
  }
}

class C8Suite extends ScalaCheckSuite {
  val impl: C8 = getImplementations().collect { case x: C8 => x }.head

  test(
    "C8: splitRecursive with empty list throws IllegalArgumentException",
  ) {
    intercept[IllegalArgumentException] {
      impl.splitRecursive(1, List())
    }
  }

  test("C8: splitRecursive with n = 0") {
    assertEquals(impl.splitRecursive(0, List(1, 2, 3)), (List(), List(1, 2, 3)))
  }

  test("C8: splitRecursive with n = list.length") {
    assertEquals(impl.splitRecursive(3, List(1, 2, 3)), (List(1, 2, 3), List()))
  }

  test("C8: splitRecursive with n = 1") {
    assertEquals(impl.splitRecursive(1, List(1, 2, 3)), (List(1), List(2, 3)))
  }

  test("C8: splitRecursive with n in the middle") {
    assertEquals(
      impl.splitRecursive(2, List(1, 2, 3, 4)),
      (List(1, 2), List(3, 4)),
    )
  }

  property("C8: splitRecursive preserves all elements") {
    forAll { (xs: List[Int], n: Int) =>
      (xs.nonEmpty && n >= 0 && n <= xs.length) ==> {
        val (left, right) = impl.splitRecursive(n, xs)
        left.length == n && left ++ right == xs
      }
    }
  }
}

class C9Suite extends ScalaCheckSuite {
  val impl: C9 = getImplementations().collect { case x: C9 => x }.head

  test("C9: decode of empty list") {
    assertEquals(impl.decode(List()), List())
  }

  test("C9: decode of single element encoding") {
    assertEquals(impl.decode(List(('a', 1))), List('a'))
  }

  test("C9: decode of multiple count encoding") {
    assertEquals(
      impl.decode(List(('a', 2), ('b', 3))),
      List('a', 'a', 'b', 'b', 'b'),
    )
  }

  test("C9: decode of mixed counts") {
    assertEquals(
      impl.decode(List(('a', 2), ('b', 1), ('c', 3))),
      List('a', 'a', 'b', 'c', 'c', 'c'),
    )
  }

  property("C9: decode expands encoded list correctly") {
    forAll { (xs: List[(Char, Int)]) =>
      val validXs = xs.filter { case (_, n) => n > 0 && n <= 100 }
      if (validXs.isEmpty) {
        impl.decode(validXs) == List()
      } else {
        impl.decode(validXs) == validXs.flatMap { case (e, n) =>
          List.fill(n)(e)
        }
      }
    }
  }
}

class C10Suite extends ScalaCheckSuite {
  val impl: C10 = getImplementations().collect { case x: C10 => x }.head

  test("C10: takeWhileStrictlyIncreasing of empty list") {
    assertEquals(impl.takeWhileStrictlyIncreasing(List()), List())
  }

  test("C10: takeWhileStrictlyIncreasing of single element list") {
    assertEquals(impl.takeWhileStrictlyIncreasing(List(1)), List(1))
  }

  test("C10: takeWhileStrictlyIncreasing of strictly increasing list") {
    assertEquals(
      impl.takeWhileStrictlyIncreasing(List(1, 2, 3, 4, 5)),
      List(1, 2, 3, 4, 5),
    )
  }

  test("C10: takeWhileStrictlyIncreasing with decreasing elements") {
    assertEquals(
      impl.takeWhileStrictlyIncreasing(List(1, 3, 5, 4, 7)),
      List(1, 3, 5),
    )
  }

  test("C10: takeWhileStrictlyIncreasing with equal elements") {
    assertEquals(
      impl.takeWhileStrictlyIncreasing(List(1, 2, 3, 3, 4)),
      List(1, 2, 3),
    )
  }
}

class D1Suite extends ScalaCheckSuite {
  val impl: D1 = getImplementations().collect { case x: D1 => x }.head

  test(
    "D1: multiplies each element of the list by each component of the vector",
  ) {
    val vector = Vector(2, 3, 4)
    val list = List(1, 2, 3)
    val expected = Vector(
      List(2, 4, 6), // original list * 2
      List(3, 6, 9), // original list * 3
      List(4, 8, 12), // original list * 4
    )

    assertEquals(impl.func(vector, list), expected)
  }

  test("D1: handles empty vector") {
    val vector = Vector.empty[Int]
    val list = List(1, 2, 3)

    assertEquals(impl.func(vector, list), Vector.empty[List[Int]])
  }

  test("D1: handles empty list") {
    val vector = Vector(1, 2, 3)
    val list = List.empty[Int]

    val expected = vector.map(_ => List.empty[Int])
    assertEquals(impl.func(vector, list), expected)
  }

  test("D1: handles both empty vector and list") {
    val vector = Vector.empty[Int]
    val list = List.empty[Int]

    assertEquals(impl.func(vector, list), Vector.empty[List[Int]])
  }

  test("D1: handles negative numbers correctly") {
    val vector = Vector(-1, -2, 3)
    val list = List(-4, 5, -6)
    val expected = Vector(
      List(4, -5, 6), // -1 * [-4, 5, -6]
      List(8, -10, 12), // -2 * [-4, 5, -6]
      List(-12, 15, -18), // 3 * [-4, 5, -6]
    )

    assertEquals(impl.func(vector, list), expected)
  }

  test("D1: handles zeros correctly") {
    val vector = Vector(0, 1, 2)
    val list = List(0, 1, 2)
    val expected = Vector(
      List(0, 0, 0), // 0 * [0, 1, 2]
      List(0, 1, 2), // 1 * [0, 1, 2]
      List(0, 2, 4), // 2 * [0, 1, 2]
    )

    assertEquals(impl.func(vector, list), expected)
  }

  property("D1: result vector size equals input vector size") {
    forAll { (vector: Vector[Int], list: List[Int]) =>
      impl.func(vector, list).size == vector.size
    }
  }

  property("D1: each result list size equals input list size") {
    forAll { (vector: Vector[Int], list: List[Int]) =>
      val result = impl.func(vector, list)
      vector.isEmpty || result.forall(_.size == list.size)
    }
  }

  property("D1: each element is correctly multiplied") {
    forAll { (vector: Vector[Int], list: List[Int]) =>
      val result = impl.func(vector, list)

      vector.indices.forall { i =>
        i >= result.length || list.indices.forall { j =>
          j >= result(i).length || result(i)(j) == vector(i) * list(j)
        }
      }
    }
  }

  property("D1: handles vectors with repeated values") {
    val genRepeatedVector = for {
      value <- Gen.choose(-100, 100)
      size <- Gen.choose(1, 50)
    } yield Vector.fill(size)(value)

    forAll(genRepeatedVector, Gen.listOf(Gen.choose(-100, 100))) {
      (vector, list) =>
        val result = impl.func(vector, list)

        vector.indices.forall { i =>
          i >= result.length || list.indices.forall { j =>
            j >= result(i).length || result(i)(j) == vector(i) * list(j)
          }
        }
    }
  }

  property("D1: handles lists with repeated values") {
    val genRepeatedList = for {
      value <- Gen.choose(-100, 100)
      size <- Gen.choose(1, 50)
    } yield List.fill(size)(value)

    forAll(
      Gen.listOf(Gen.choose(-100, 100)).map(Vector(_: _*)),
      genRepeatedList,
    ) { (vector, list) =>
      val result = impl.func(vector, list)

      vector.indices.forall { i =>
        i >= result.length || list.indices.forall { j =>
          j >= result(i).length || result(i)(j) == vector(i) * list(j)
        }
      }
    }
  }

  test("D1: preserves list elements when vector contains 1") {
    val vector = Vector(1, 1, 1)
    val list = List(10, 20, 30)
    val expected = Vector(
      List(10, 20, 30),
      List(10, 20, 30),
      List(10, 20, 30),
    )

    assertEquals(impl.func(vector, list), expected)
  }

  test("D1: produces zeros when vector contains 0") {
    val vector = Vector(0, 0, 0)
    val list = List(10, 20, 30)
    val expected = Vector(
      List(0, 0, 0),
      List(0, 0, 0),
      List(0, 0, 0),
    )

    assertEquals(impl.func(vector, list), expected)
  }

  test("D1: specific corner cases") {
    val testCases = List(
      (Vector(1), List(1), Vector(List(1))),
      (Vector(Int.MaxValue), List(1), Vector(List(Int.MaxValue))),
      (Vector(Int.MinValue), List(1), Vector(List(Int.MinValue))),
      (Vector(1), List(Int.MaxValue), Vector(List(Int.MaxValue))),
      (Vector(1), List(Int.MinValue), Vector(List(Int.MinValue))),
    )

    testCases.foreach { case (vector, list, expected) =>
      assertEquals(impl.func(vector, list), expected)
    }
  }

  property("D1: handles inputs of different sizes") {
    val genSizedVector = Gen
      .choose(0, 20)
      .flatMap(size =>
        Gen.listOfN(size, Gen.choose(-100, 100)).map(Vector(_: _*)),
      )
    val genSizedList = Gen
      .choose(0, 20)
      .flatMap(size => Gen.listOfN(size, Gen.choose(-100, 100)))

    forAll(genSizedVector, genSizedList) { (vector, list) =>
      val result = impl.func(vector, list)

      result.size == vector.size &&
      (vector.isEmpty || result.forall(_.size == list.size)) &&
      vector.indices.forall { i =>
        list.indices.forall { j =>
          i >= result.length || j >= result(i).length || result(i)(j) == vector(
            i,
          ) * list(j)
        }
      }
    }
  }

  property("D1: function is referentially transparent") {
    forAll { (vector: Vector[Int], list: List[Int]) =>
      val result1 = impl.func(vector, list)
      val result2 = impl.func(vector, list)

      result1 == result2
    }
  }
}

class D2Suite extends ScalaCheckSuite {
  val impl: D2 = getImplementations().collect { case x: D2 => x }.head

  test("D2: finds maximum in a list of positive integers") {
    val list = List(1, 5, 3, 9, 2, 7)
    assertEquals(impl.max(list), 9)
  }

  test("D2: finds maximum in a list of negative integers") {
    val list = List(-5, -3, -1, -10, -7)
    assertEquals(impl.max(list), -1)
  }

  test("D2: finds maximum in a mixed list") {
    val list = List(-5, 3, -1, 10, -7)
    assertEquals(impl.max(list), 10)
  }

  test("D2: handles list with a single element") {
    val list = List(42)
    assertEquals(impl.max(list), 42)
  }

  test("D2: handles list with duplicate maximum values") {
    val list = List(1, 5, 9, 3, 9, 2, 7)
    assertEquals(impl.max(list), 9)
  }

  test("D2: handles list with values close to Int bounds") {
    val list =
      List(Int.MaxValue - 10, Int.MaxValue - 5, Int.MaxValue, Int.MinValue, 0)
    assertEquals(impl.max(list), Int.MaxValue)
  }

  test("D2: throws exception on empty list") {
    val list = List.empty[Int]
    intercept[UnsupportedOperationException] {
      impl.max(list)
    }
  }

  test("D2: handles list with zeros") {
    val list = List(0, 0, 0)
    assertEquals(impl.max(list), 0)
  }

  property("D2: maximum is greater than or equal to all elements") {
    val nonEmptyIntList = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]())

    forAll(nonEmptyIntList) { list =>
      val max = impl.max(list)
      list.forall(x => max >= x)
    }
  }

  property("D2: maximum exists in the list") {
    val nonEmptyIntList = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]())

    forAll(nonEmptyIntList) { list =>
      val max = impl.max(list)
      list.contains(max)
    }
  }

  property("D2: result matches standard library max") {
    val nonEmptyIntList = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]())

    forAll(nonEmptyIntList) { list =>
      impl.max(list) == list.max
    }
  }

  property("D2: function is referentially transparent") {
    val nonEmptyIntList = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]())

    forAll(nonEmptyIntList) { list =>
      val result1 = impl.max(list)
      val result2 = impl.max(list)
      result1 == result2
    }
  }

  property("D2: maximum of identical elements is that element") {
    forAll(Gen.choose(0, 100)) { n =>
      val list = List.fill(10)(n)
      impl.max(list) == n
    }
  }

  test("D2: handles ascending list") {
    val list = List(1, 2, 3, 4, 5)
    assertEquals(impl.max(list), 5)
  }

  test("D2: handles descending list") {
    val list = List(5, 4, 3, 2, 1)
    assertEquals(impl.max(list), 5)
  }

  test("D2: handles zigzag pattern") {
    val list = List(1, 10, 2, 9, 3, 8, 4, 7, 5, 6)
    assertEquals(impl.max(list), 10)
  }

  test("D2: handles reasonably large lists efficiently") {
    val list = (1 to 10000).toList ++ List(Int.MaxValue) ++ (1 to 10000).toList
    assertEquals(impl.max(list), Int.MaxValue)
  }

  test("D2: implementation should handle first element correctly") {
    val list = List(100, 1, 2, 3, 4, 5)
    assertEquals(impl.max(list), 100)
  }
}

class D3Suite extends ScalaCheckSuite {
  val impl: D3 = getImplementations().collect { case x: D3 => x }.head

  test("D3: 1.to(10).reduceLeft(_ * _) equals 3628800") {
    val result = 1.to(10).reduceLeft(_ * _)
    assertEquals(result, 3628800)
  }

  test("D3: factorial of small numbers") {
    val testCases = Seq(
      (0, BigInt(1)), // 0! = 1 (special case)
      (1, BigInt(1)), // 1! = 1
      (2, BigInt(2)), // 2! = 2
      (3, BigInt(6)), // 3! = 6
      (4, BigInt(24)), // 4! = 24
      (5, BigInt(120)), // 5! = 120
      (10, BigInt(3628800)), // 10! = 3628800
    )

    testCases.foreach { case (n, expected) =>
      assertEquals(impl.func(n), expected, s"factorial($n)")
    }
  }

  test("D3: factorial of larger numbers") {
    val testCases = Seq(
      (12, BigInt("479001600")),
      (15, BigInt("1307674368000")),
      (20, BigInt("2432902008176640000")),
    )

    testCases.foreach { case (n, expected) =>
      assertEquals(impl.func(n), expected, s"factorial($n)")
    }
  }

  test(
    "D3: factorial of negative numbers should throw IllegalArgumentException",
  ) {
    val negativeInputs = Seq(-1, -5, -10)

    negativeInputs.foreach { n =>
      intercept[IllegalArgumentException] {
        impl.func(n)
      }
    }
  }

  test("D3: factorial of 0 is 1") {
    assertEquals(impl.func(0), BigInt(1))
  }

  property("D3: factorial(n+1) = (n+1) * factorial(n)") {
    val genSmallInt = Gen.choose(0, 20)

    forAll(genSmallInt) { n =>
      val factN = impl.func(n)
      val factNPlus1 = impl.func(n + 1)
      factNPlus1 == BigInt(n + 1) * factN
    }
  }

  property("D3: factorial grows faster than exponential") {
    val genPositiveInt = Gen.choose(5, 100)

    forAll(genPositiveInt) { n =>
      impl.func(n) > BigInt(2).pow(n)
    }
  }

  property("D3: factorial is always positive") {
    val genNonNegativeInt = Gen.choose(0, 1000)

    forAll(genNonNegativeInt) { n =>
      impl.func(n) > BigInt(0)
    }
  }

  property("D3: factorial matches recursive definition") {
    // Restrain to moderate values to avoid stack overflow on recursive implementation
    val genNonNegativeInt = Gen.choose(0, 15)

    def recursiveFactorial(n: Int): BigInt = {
      if (n <= 1) BigInt(1)
      else BigInt(n) * recursiveFactorial(n - 1)
    }

    forAll(genNonNegativeInt) { n =>
      impl.func(n) == recursiveFactorial(n)
    }
  }

  test("D3: handles larger inputs without overflow") {
    val result = impl.func(25)
    val expected = BigInt("15511210043330985984000000")
    assertEquals(result, expected)
  }

  test("D3: handles reasonably large factorial efficiently") {
    val n = 50
    val result = impl.func(n)
    val expected = BigInt(
      "30414093201713378043612608166064768844377641568960512000000000000",
    )
    assertEquals(result, expected)
  }

  test("D3: extremely large values don't cause stack overflow") {
    impl.func(100)
    assert(true) // If we get here without exception, the test passes
  }
}

class D4Suite extends ScalaCheckSuite {
  val impl: D4 = getImplementations().collect { case x: D4 => x }.head

  test("D4: powers of 2 for small exponents") {
    val testCases = Seq(
      (0, BigInt(1)), // 2^0 = 1
      (1, BigInt(2)), // 2^1 = 2
      (2, BigInt(4)), // 2^2 = 4
      (3, BigInt(8)), // 2^3 = 8
      (4, BigInt(16)), // 2^4 = 16
      (5, BigInt(32)), // 2^5 = 32
      (10, BigInt(1024)), // 2^10 = 1024
    )

    testCases.foreach { case (n, expected) =>
      assertEquals(impl.baseTwoPower(n), expected, s"2^$n")
    }
  }

  test("D4: powers of 2 for medium exponents") {
    val testCases = Seq(
      (16, BigInt("65536")),
      (20, BigInt("1048576")),
      (30, BigInt("1073741824")),
    )

    testCases.foreach { case (n, expected) =>
      assertEquals(impl.baseTwoPower(n), expected, s"2^$n")
    }
  }

  test(
    "D4: baseTwoPower of negative exponents should throw IllegalArgumentException",
  ) {
    val negativeInputs = Seq(-1, -5, -10)

    negativeInputs.foreach { n =>
      intercept[IllegalArgumentException] {
        impl.baseTwoPower(n)
      }
    }
  }

  test("D4: 2^0 is 1") {
    assertEquals(impl.baseTwoPower(0), BigInt(1))
  }

  property("D4: 2^(n+1) = 2 * 2^n") {
    val genNonNegativeInt = Gen.choose(0, 1000)

    forAll(genNonNegativeInt) { n =>
      val power2N = impl.baseTwoPower(n)
      val power2NPlus1 = impl.baseTwoPower(n + 1)
      power2NPlus1 == BigInt(2) * power2N
    }
  }

  property("D4: 2^n is always positive") {
    val genNonNegativeInt = Gen.choose(0, 1000)

    forAll(genNonNegativeInt) { n =>
      impl.baseTwoPower(n) > BigInt(0)
    }
  }

  property("D4: 2^n matches reference calculation") {
    val genNonNegativeInt = Gen.choose(0, 1000)

    forAll(genNonNegativeInt) { n =>
      impl.baseTwoPower(n) == BigInt(2).pow(n)
    }
  }

  property("D4: 2^n is even for all n > 0") {
    val genPositiveInt = Gen.choose(1, 1000)

    forAll(genPositiveInt) { n =>
      impl.baseTwoPower(n) % BigInt(2) == BigInt(0)
    }
  }

  property("D4: 2^n > n for all n > 0") {
    val genPositiveInt = Gen.choose(1, 1000)

    forAll(genPositiveInt) { n =>
      impl.baseTwoPower(n) > BigInt(n)
    }
  }

  test("D4: handles larger inputs without overflow") {
    val result = impl.baseTwoPower(100)
    val expected = BigInt("1267650600228229401496703205376")
    assertEquals(result, expected)
  }

  test("D4: handles very large exponent efficiently") {
    val result = impl.baseTwoPower(1000)
    val expected = BigInt(2).pow(1000)
    assertEquals(result, expected)
  }

  test("D4: extremely large exponents don't cause stack overflow") {
    impl.baseTwoPower(10000)
    assert(true) // If we get here without exception, the test passes
  }

  test("D4: verifies basic properties of powers of 2") {
    // 2^3 * 2^2 = 2^5
    assertEquals(
      impl.baseTwoPower(3) * impl.baseTwoPower(2),
      impl.baseTwoPower(5),
    )

    // 2^10 / 2^5 = 2^5
    assertEquals(
      impl.baseTwoPower(10) / impl.baseTwoPower(5),
      impl.baseTwoPower(5),
    )

    // 2^0 = 1
    assertEquals(impl.baseTwoPower(0), BigInt(1))

    // 2^1 = 2
    assertEquals(impl.baseTwoPower(1), BigInt(2))
  }

  test(
    "D4: binary representation of 2^n has n+1 digits with only first bit set",
  ) {
    val testCases = Seq(
      (0, "1"), // 2^0 = 1 = 1 in binary
      (1, "10"), // 2^1 = 2 = 10 in binary
      (2, "100"), // 2^2 = 4 = 100 in binary
      (3, "1000"), // 2^3 = 8 = 1000 in binary
      (4, "10000"), // 2^4 = 16 = 10000 in binary
      (5, "100000"), // 2^5 = 32 = 100000 in binary
    )

    testCases.foreach { case (n, binaryStr) =>
      assertEquals(impl.baseTwoPower(n).toString(2), binaryStr)
    }
  }
}

class D5Suite extends ScalaCheckSuite {
  val impl: D5 = getImplementations().collect { case x: D5 => x }.head

  test("D5: catSpace works with the given example") {
    val input = Vector("I", "have", "a", "dream")
    val expected = "I have a dream"
    assertEquals(impl.catSpace(input), expected)
  }

  test("D5: catSpace with empty sequence returns empty string") {
    val input = Seq.empty[String]
    assertEquals(impl.catSpace(input), "")
  }

  test("D5: catSpace with single element returns that element") {
    val input = Seq("Hello")
    assertEquals(impl.catSpace(input), "Hello")
  }

  test("D5: catSpace handles elements with whitespace") {
    val input = Seq("Hello,", "world!", "How", "are  you?")
    val expected = "Hello, world! How are  you?"
    assertEquals(impl.catSpace(input), expected)
  }

  test("D5: catSpace works with different sequence types") {
    val inputVector = Vector("a", "b", "c")
    val inputList = List("a", "b", "c")
    val expected = "a b c"

    assertEquals(impl.catSpace(inputVector), expected)
    assertEquals(impl.catSpace(inputList), expected)
  }

  test("D5: catSpace handles empty strings correctly") {
    val input = Seq("First", "", "Third")
    val expected = "First  Third"
    assertEquals(impl.catSpace(input), expected)
  }

  test("D5: catSpace handles long sequences efficiently") {
    val input = (1 to 1000).map(_.toString)
    val result = impl.catSpace(input)

    assert(result.startsWith("1 2 3 4"))
    assert(result.endsWith("997 998 999 1000"))
    assertEquals(result.split(" ").length, 1000)
  }

  property("D5: number of spaces is one less than number of elements") {
    val nonEmptyStringSeq =
      Gen.nonEmptyListOf(Gen.nonEmptyStringOf(Gen.alphaNumChar))

    forAll(nonEmptyStringSeq) { strings =>
      val spaces = impl.catSpace(strings).count(_ == ' ')
      spaces == strings.length - 1
    }
  }

  property("D5: all original strings are in the result") {
    val nonEmptyDistinctStringSeq = Gen
      .nonEmptyListOf(Gen.alphaNumStr.suchThat(_.length > 3))
      .map(_.filter(_.nonEmpty).distinct)

    forAll(nonEmptyDistinctStringSeq) { strings =>
      val result = impl.catSpace(strings)
      strings.forall(str => result.contains(str))
    }
  }

  property("D5: total length is sum of string lengths plus spaces") {
    val nonEmptyStringSeq = Gen.nonEmptyListOf(Gen.alphaNumStr)

    forAll(nonEmptyStringSeq) { strings =>
      val result = impl.catSpace(strings)
      val expectedLength = strings.map(_.length).sum + (strings.length - 1)
      result.length == expectedLength
    }
  }

  test("D5: catSpace handles special characters correctly") {
    val input = Seq("Hello,", "世界!", "Καλημέρα", "❤️")
    val expected = "Hello, 世界! Καλημέρα ❤️"
    assertEquals(impl.catSpace(input), expected)
  }

  test("D5: catSpace works with strings containing spaces") {
    val input = Seq("Hello world", "how are", "you today?")
    val expected = "Hello world how are you today?"
    assertEquals(impl.catSpace(input), expected)
  }

  test("D5: implementation handles consecutive spaces correctly") {
    val input = Seq("This", "  has", " spaces ")
    val expected = "This   has  spaces "
    assertEquals(impl.catSpace(input), expected)
  }

  test("D5: catSpace handles sequences with large strings") {
    val largeString = "a" * 10000
    val input = Seq(largeString, largeString, largeString)
    val result = impl.catSpace(input)

    assertEquals(result.length, 3 * largeString.length + 2)
    assert(result.startsWith(largeString))
  }

  test("D5: catSpace handles null strings gracefully") {
    val inputWithNull = Seq("Hello", null, "World")

    val result = impl.catSpace(inputWithNull)
    assert(result.contains("null"))
  }
}

class D6Suite extends ScalaCheckSuite {
  val impl: D6 = getImplementations().collect { case x: D6 => x }.head

  test("D6: reverse a list of integers") {
    val input = List(1, 2, 3, 4, 5)
    val expected = List(5, 4, 3, 2, 1)
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse a list of strings") {
    val input = List("a", "b", "c", "d")
    val expected = List("d", "c", "b", "a")
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse works with different types") {
    val input = List(1, "a", true, 3.14)
    val expected = List(3.14, true, "a", 1)
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse of empty list is empty list") {
    val input = List.empty[Int]
    val expected = List.empty[Int]
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse of single element list is the same list") {
    val input = List(42)
    val expected = List(42)
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse handles lists with duplicate elements") {
    val input = List(1, 2, 2, 3, 1)
    val expected = List(1, 3, 2, 2, 1)
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse implementation preserves order correctly") {
    val input = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val expected = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse handles large lists efficiently") {
    val input = (1 to 10000).toList
    val expected = (10000 to 1 by -1).toList
    assertEquals(impl.reverse(input), expected)
  }

  property("D6: reversing twice returns the original list") {
    forAll { (list: List[Int]) =>
      impl.reverse(impl.reverse(list)) == list
    }
  }

  property("D6: first element becomes last and last becomes first") {
    val nonEmptyList = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]())
    forAll(nonEmptyList) { list =>
      val reversed = impl.reverse(list)
      (list.headOption == reversed.lastOption) &&
      (list.lastOption == reversed.headOption)
    }
  }

  property("D6: length is preserved after reversal") {
    forAll { (list: List[String]) =>
      impl.reverse(list).length == list.length
    }
  }

  property("D6: all elements are preserved after reversal") {
    forAll { (list: List[Int]) =>
      list.forall(elem => impl.reverse(list).contains(elem))
    }
  }

  property("D6: elements are in exactly reversed order") {
    forAll { (list: List[Int]) =>
      impl.reverse(list) == list.reverse
    }
  }

  test("D6: reverse works with nested lists") {
    val input = List(List(1, 2), List(3, 4), List(5, 6))
    val expected = List(List(5, 6), List(3, 4), List(1, 2))
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse handles lists with null values") {
    val input = List("a", null, "c", null)
    val expected = List(null, "c", null, "a")
    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse works with custom case classes") {
    case class Person(name: String, age: Int)

    val alice = Person("Alice", 30)
    val bob = Person("Bob", 25)
    val charlie = Person("Charlie", 35)

    val input = List(alice, bob, charlie)
    val expected = List(charlie, bob, alice)

    assertEquals(impl.reverse(input), expected)
  }

  test("D6: reverse is referentially transparent") {
    val list = List(1, 2, 3)
    val result1 = impl.reverse(list)
    val result2 = impl.reverse(list)

    assertEquals(result1, result2)
    assertEquals(list, List(1, 2, 3))
  }

  property("D6: reverse(a ++ b) == reverse(b) ++ reverse(a)") {
    forAll { (a: List[Int], b: List[Int]) =>
      impl.reverse(a ++ b) == impl.reverse(b) ++ impl.reverse(a)
    }
  }
}

class D7Suite extends ScalaCheckSuite {
  val impl: D7 = getImplementations().collect { case x: D7 => x }.head

  test("D7: firstColumn extracts first column from regular matrix") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )
    val expected = List(1, 4, 7)
    assertEquals(impl.firstColumn(matrix), expected)
  }

  test("D7: firstColumn extracts from rectangular matrix") {
    val matrix = List(
      List(1, 2, 3, 4),
      List(5, 6, 7, 8),
      List(9, 10, 11, 12),
    )
    val expected = List(1, 5, 9)
    assertEquals(impl.firstColumn(matrix), expected)
  }

  test("D7: column extracts specific column from regular matrix") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )

    assertEquals(impl.column(matrix, 0), List(1, 4, 7)) // First column
    assertEquals(impl.column(matrix, 1), List(2, 5, 8)) // Second column
    assertEquals(impl.column(matrix, 2), List(3, 6, 9)) // Third column
  }

  test("D7: column extracts from rectangular matrix") {
    val matrix = List(
      List(1, 2, 3, 4),
      List(5, 6, 7, 8),
      List(9, 10, 11, 12),
    )

    assertEquals(impl.column(matrix, 0), List(1, 5, 9))
    assertEquals(impl.column(matrix, 1), List(2, 6, 10))
    assertEquals(impl.column(matrix, 2), List(3, 7, 11))
    assertEquals(impl.column(matrix, 3), List(4, 8, 12))
  }

  test("D7: firstColumn with single-column matrix") {
    val matrix = List(
      List(1),
      List(2),
      List(3),
    )
    val expected = List(1, 2, 3)
    assertEquals(impl.firstColumn(matrix), expected)
  }

  test("D7: firstColumn with single-row matrix") {
    val matrix = List(
      List(1, 2, 3, 4),
    )
    val expected = List(1)
    assertEquals(impl.firstColumn(matrix), expected)
  }

  test("D7: column with single-element matrix") {
    val matrix = List(List(42))
    assertEquals(impl.column(matrix, 0), List(42))

    intercept[IndexOutOfBoundsException] {
      impl.column(matrix, 1)
    }
  }

  test("D7: firstColumn with empty matrix") {
    val emptyMatrix = List.empty[List[Int]]
    assertEquals(impl.firstColumn(emptyMatrix), List.empty[Int])
  }

  test("D7: column with empty matrix") {
    val emptyMatrix = List.empty[List[Int]]
    assertEquals(impl.column(emptyMatrix, 0), List.empty[Int])

    // Any column index should return empty list for empty matrix
    assertEquals(impl.column(emptyMatrix, 5), List.empty[Int])
  }

  test("D7: firstColumn with matrix containing empty rows") {
    val matrix = List(
      List(1, 2, 3),
      List.empty[Int],
      List(7, 8, 9),
    )

    intercept[IllegalArgumentException | NoSuchElementException] {
      impl.firstColumn(matrix)
    }
  }

  test("D7: column with negative index") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )

    intercept[IllegalArgumentException | NoSuchElementException] {
      impl.column(matrix, -1)
    }
  }

  test("D7: column with index out of bounds") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )

    intercept[IndexOutOfBoundsException] {
      impl.column(matrix, 3) // Only columns 0, 1, 2 exist
    }
  }

  test("D7: firstColumn with rows of different lengths") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5),
      List(7, 8, 9, 10),
    )

    val expected = List(1, 4, 7)
    assertEquals(impl.firstColumn(matrix), expected)
  }

  test("D7: column with rows of different lengths") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5),
      List(7, 8, 9, 10),
    )

    assertEquals(impl.column(matrix, 0), List(1, 4, 7))
    assertEquals(impl.column(matrix, 1), List(2, 5, 8))

    intercept[IndexOutOfBoundsException] {
      impl.column(matrix, 2)
    }
  }

  val genMatrix = for {
    rows <- Gen.choose(1, 10)
    cols <- Gen.choose(1, 10)
    matrix <- Gen.listOfN(rows, Gen.listOfN(cols, Gen.choose(-100, 100)))
  } yield matrix

  property("D7: firstColumn(matrix) == column(matrix, 0)") {
    forAll(genMatrix) { matrix =>
      impl.firstColumn(matrix) == impl.column(matrix, 0)
    }
  }

  property("D7: column extraction works for all valid indices") {
    val genSquareMatrix = for {
      size <- Gen.choose(1, 10)
      matrix <- Gen.listOfN(size, Gen.listOfN(size, Gen.choose(-100, 100)))
    } yield matrix

    forAll(genSquareMatrix) { matrix =>
      val colIndices = matrix.head.indices
      colIndices.forall { i =>
        val col = impl.column(matrix, i)
        col.length == matrix.length && // Column has same length as number of rows
        matrix.indices.forall(rowIdx =>
          col(rowIdx) == matrix(rowIdx)(i),
        ) // Each element is from the right column
      }
    }
  }

  property("D7: transposing twice preserves columns") {
    forAll(genMatrix) { matrix =>
      // Create simple transpose function for testing
      def transpose(m: List[List[Int]]): List[List[Int]] = {
        val colCount = m.map(_.length).min // Find minimum row length
        (0 until colCount).map(i => impl.column(m, i)).toList
      }

      val transposed = transpose(matrix)
      val originalCol = impl.column(matrix, 0)
      val colAfterTwoTransposes = impl.column(transpose(transposed), 0)

      originalCol == colAfterTwoTransposes
    }
  }

  property("D7: column extraction matches direct indexing") {
    forAll(genMatrix) { matrix =>
      val colIdx = Gen.choose(0, matrix.head.length - 1).sample.getOrElse(0)
      val col = impl.column(matrix, colIdx)

      col.length == matrix.length &&
      col.indices.forall(i => col(i) == matrix(i)(colIdx))
    }
  }

  test("D7: handles large matrices efficiently") {
    val largeMatrix = List.fill(100)(List.fill(100)(0).zipWithIndex.map(_._2))
    assertEquals(impl.firstColumn(largeMatrix), List.fill(100)(0))
    assertEquals(impl.column(largeMatrix, 99), List.fill(100)(99))
  }
}

class D8Suite extends ScalaCheckSuite {
  val impl: D8 = getImplementations().collect { case x: D8 => x }.head

  test("D8: extract diagonal from a simple matrix") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )
    val expected = List(1, 5, 9)
    assertEquals(impl.diagonal(matrix), expected)
  }

  test(
    "D8: extract diagonal from a rectangular matrix (more rows than columns)",
  ) {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
      List(10, 11, 12),
    )
    val expected = List(1, 5, 9)
    assertEquals(impl.diagonal(matrix), expected)
  }

  test(
    "D8: extract diagonal from a rectangular matrix (more columns than rows)",
  ) {
    val matrix = List(
      List(1, 2, 3, 4),
      List(5, 6, 7, 8),
      List(9, 10, 11, 12),
    )
    val expected = List(1, 6, 11)
    assertEquals(impl.diagonal(matrix), expected)
  }

  test("D8: extract diagonal from a single-element matrix") {
    val matrix = List(List(42))
    val expected = List(42)
    assertEquals(impl.diagonal(matrix), expected)
  }

  test("D8: extract diagonal from an empty matrix") {
    val matrix = List.empty[List[Int]]
    val expected = List.empty[Int]
    assertEquals(impl.diagonal(matrix), expected)
  }

  test("D8: extract diagonal from a matrix with varying row sizes") {
    val matrix = List(
      List(1, 2, 3),
      List.empty[Int],
      List(7, 8, 9, 10),
    )

    intercept[IllegalArgumentException] {
      impl.diagonal(matrix)
    }
  }

  test("D8: matrix with zero values on diagonal") {
    val matrix = List(
      List(0, 1, 2),
      List(3, 0, 4),
      List(5, 6, 0),
    )
    val expected = List(0, 0, 0)
    assertEquals(impl.diagonal(matrix), expected)
  }

  test("D8: matrix with negative values on diagonal") {
    val matrix = List(
      List(-1, 1, 2),
      List(3, -5, 4),
      List(5, 6, -9),
    )
    val expected = List(-1, -5, -9)
    assertEquals(impl.diagonal(matrix), expected)
  }

  val genMatrix = for {
    rows <- Gen.choose(1, 10)
    cols <- Gen.choose(1, 10)
    matrix <- Gen.listOfN(rows, Gen.listOfN(cols, Gen.choose(-100, 100)))
  } yield matrix

  val genSquareMatrix = for {
    size <- Gen.choose(1, 10)
    matrix <- Gen.listOfN(size, Gen.listOfN(size, Gen.choose(-100, 100)))
  } yield matrix

  property("D8: diagonal length equals matrix size for square matrices") {
    forAll(genSquareMatrix) { matrix =>
      impl.diagonal(matrix).length == matrix.length
    }
  }

  property("D8: diagonal length is min(rows, cols) for rectangular matrices") {
    forAll(genMatrix) { matrix =>
      val rows = matrix.length
      val cols = matrix.head.length
      impl.diagonal(matrix).length == math.min(rows, cols)
    }
  }

  property(
    "D8: diagonal elements are at positions where row index equals column index",
  ) {
    forAll(genMatrix) { matrix =>
      val diagonal = impl.diagonal(matrix)
      diagonal.indices.forall(i => diagonal(i) == matrix(i)(i))
    }
  }

  property("D8: diagonal is not affected by non-diagonal elements") {
    forAll(genSquareMatrix) { matrix =>
      val originalDiagonal = impl.diagonal(matrix)

      val modifiedMatrix = matrix.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (element, j) =>
          if (i == j) element else element + 1000
        }
      }

      impl.diagonal(modifiedMatrix) == originalDiagonal
    }
  }

  property("D8: matrix with all same values has diagonal of all that value") {
    forAll(Gen.choose(1, 10), Gen.choose(-100, 100)) { (size, value) =>
      val matrix = List.fill(size)(List.fill(size)(value))
      impl.diagonal(matrix) == List.fill(size)(value)
    }
  }
}

class D9Suite extends ScalaCheckSuite {
  val impl: D9 = getImplementations().collect { case x: D9 => x }.head

  test("D9: matrix with a zero row returns true") {
    val matrix = List(
      List(1, 2, 3),
      List(0, 0, 0),
      List(7, 8, 9),
    )
    assert(impl.hasZeroRow(matrix))
  }

  test("D9: matrix with no zero row returns false") {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )
    assert(!impl.hasZeroRow(matrix))
  }

  test("D9: matrix with partial zero row returns false") {
    val matrix = List(
      List(1, 2, 3),
      List(0, 0, 1),
      List(7, 8, 9),
    )
    assert(!impl.hasZeroRow(matrix))
  }

  test("D9: matrix with multiple zero rows returns true") {
    val matrix = List(
      List(0, 0, 0),
      List(4, 5, 6),
      List(0, 0, 0),
    )
    assert(impl.hasZeroRow(matrix))
  }

  test("D9: empty matrix returns false") {
    val matrix = List.empty[List[Int]]
    assert(!impl.hasZeroRow(matrix))
  }

  test("D9: single row with all zeros returns true") {
    val matrix = List(List(0, 0, 0))
    assert(impl.hasZeroRow(matrix))
  }

  test("D9: single row with some zeros returns false") {
    val matrix = List(List(0, 1, 0))
    assert(!impl.hasZeroRow(matrix))
  }

  test("D9: single column matrix with zero row returns true") {
    val matrix = List(
      List(1),
      List(0),
      List(2),
    )
    assert(impl.hasZeroRow(matrix))
  }

  test("D9: single element zero matrix returns true") {
    val matrix = List(List(0))
    assert(impl.hasZeroRow(matrix))
  }

  test("D9: single element non-zero matrix returns false") {
    val matrix = List(List(1))
    assert(!impl.hasZeroRow(matrix))
  }

  test("D9: matrix with rows of different lengths") {
    val matrix = List(
      List(1, 2, 3),
      List(0, 0),
      List(7, 8, 9, 10),
    )

    assert(impl.hasZeroRow(matrix))

    val matrix2 = List(
      List(1, 2, 3),
      List(0, 1),
      List(7, 8, 9, 10),
    )

    assert(!impl.hasZeroRow(matrix2))
  }

  val genMatrix = for {
    rows <- Gen.choose(1, 10)
    cols <- Gen.choose(1, 10)
    matrix <- Gen.listOfN(rows, Gen.listOfN(cols, Gen.choose(-100, 100)))
  } yield matrix

  property("D9: matrix with inserted zero row returns true") {
    forAll(genMatrix) { matrix =>
      val cols = if (matrix.head.nonEmpty) matrix.head.length else 0
      val zeroRow = List.fill(cols)(0)
      val matrixWithZeroRow = matrix.take(matrix.length / 2) ++ List(
        zeroRow,
      ) ++ matrix.drop(matrix.length / 2)

      impl.hasZeroRow(matrixWithZeroRow)
    }
  }
}

class D10Suite extends ScalaCheckSuite {
  val impl: D10 = getImplementations().collect { case x: D10 => x }.head

  test("D10: small primes return true") {
    val primeNumbers = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
    primeNumbers.foreach(n => assert(impl.isPrime(n), s"$n should be prime"))
  }

  test("D10: small non-primes return false") {
    val nonPrimeNumbers = List(4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21)
    nonPrimeNumbers.foreach(n =>
      assert(!impl.isPrime(n), s"$n should not be prime"),
    )
  }

  test("D10: edge cases: 0 and 1 are not prime") {
    assert(!impl.isPrime(0))
    assert(!impl.isPrime(1))
  }

  test("D10: negative numbers are illegal values") {
    val negativeNumbers = List(-1, -2, -3, -5, -7, -11)
    negativeNumbers.foreach(n =>
      intercept[IllegalArgumentException] {
        impl.isPrime(n)
      },
    )
  }

  test("D10: larger known primes") {
    val largerPrimes = List(97, 101, 199, 251, 509, 997)
    largerPrimes.foreach(n => assert(impl.isPrime(n), s"$n should be prime"))
  }

  test("D10: larger known non-primes") {
    val largerNonPrimes = List(100, 169, 200, 256, 500, 1000)
    largerNonPrimes.foreach(n =>
      assert(!impl.isPrime(n), s"$n should not be prime"),
    )
  }

  property("D10: product of two numbers greater than 1 is never prime") {
    val genPositiveInt = Gen.choose(2, 100)

    forAll(genPositiveInt, genPositiveInt) { (a, b) =>
      !impl.isPrime(a * b)
    }
  }

  property("D10: prime numbers greater than 2 are always odd") {
    val genLargeInt = Gen.choose(3, 1000).suchThat(_ % 2 == 0)

    forAll(genLargeInt) { n =>
      !impl.isPrime(n)
    }
  }

  property(
    "D10: a number divisible by any number smaller than itself (except 1) is not prime",
  ) {
    val genPositiveInt = Gen.choose(4, 1000).suchThat { n =>
      (2 to math.sqrt(n).toInt).exists(n % _ == 0)
    }

    forAll(genPositiveInt) { n =>
      !impl.isPrime(n)
    }
  }

  property("D10: only numbers with exactly two divisors are prime") {
    val genPositiveInt = Gen.choose(2, 200)

    forAll(genPositiveInt) { n =>
      val divisors = (1 to n).count(n % _ == 0)
      impl.isPrime(n) == (divisors == 2)
    }
  }

  property(
    "D10: Fermat's Little Theorem: if p is prime, then a^p ≡ a (mod p) for any a",
  ) {
    val genPossiblePrime = Gen.choose(2, 100).suchThat(impl.isPrime)
    val genA = Gen.choose(1, 100)

    forAll(genPossiblePrime, genA) { (p, a) =>
      val aPowP = BigInt(a).modPow(p, p)
      aPowP == (a % p)
    }
  }
}

val diverseStringGen: Gen[String] = for {
  chars <- Gen.listOf(
    Gen.frequency(
      (10, Gen.alphaNumChar),
      (2, Gen.oneOf('é', 'ñ', '日', '本', '語')),
    ),
  )
} yield chars.mkString

val diverseStringListGen: Gen[List[String]] = Gen.listOf(diverseStringGen)

class D11Suite extends ScalaCheckSuite {
  val impl: D11 = getImplementations().collect { case x: D11 => x }.head

  test("D11: linesLonger with specific examples") {
    assertEquals(
      impl.linesLonger(List("a", "ab", "abc", "abcd"), 2),
      List("abc", "abcd"),
    )
    assertEquals(
      impl.linesLonger(List("a", "ab", "abc", "abcd"), 3),
      List("abcd"),
    )
    assertEquals(
      impl.linesLonger(List("a", "ab", "abc", "abcd"), 4),
      List(),
    )
    assertEquals(
      impl.linesLonger(List("aa", "bb", "cc"), 2),
      List(),
    )
    assertEquals(
      impl.linesLonger(List(), 5),
      List(),
    )
  }

  property("D11: linesLonger behaves like filter with length check") {
    forAll(diverseStringListGen, Gen.choose(-10, 30)) {
      (lines: List[String], len: Int) =>
        impl.linesLonger(lines, len) == lines.filter(_.length > len)
    }
  }

  property("D11: linesLonger is idempotent with the same length parameter") {
    forAll(diverseStringListGen, Gen.choose(-10, 30)) {
      (lines: List[String], len: Int) =>
        val once = impl.linesLonger(lines, len)
        val twice = impl.linesLonger(once, len)
        once == twice
    }
  }

  property(
    "D11: linesLonger with increasing length produces a subset of results",
  ) {
    forAll(diverseStringListGen, Gen.choose(-10, 20), Gen.choose(0, 10)) {
      (lines: List[String], len1: Int, lenDiff: Int) =>
        val len2 = len1 + lenDiff
        val result1 = impl.linesLonger(lines, len1)
        val result2 = impl.linesLonger(lines, len2)
        result2.toSet.subsetOf(result1.toSet)
    }
  }

  property("D11: linesLonger with Int.MaxValue returns empty list") {
    forAll(diverseStringListGen) { (lines: List[String]) =>
      impl.linesLonger(lines, Int.MaxValue).isEmpty
    }
  }

  property("D11: linesLonger with Int.MinValue returns all strings") {
    forAll(diverseStringListGen) { (lines: List[String]) =>
      impl.linesLonger(lines, Int.MinValue) == lines
    }
  }
}

class D12Suite extends ScalaCheckSuite {
  val impl: D12 = getImplementations().collect { case x: D12 => x }.head

  val nonEmptyDiverseStringListGen: Gen[List[String]] =
    Gen.nonEmptyListOf(diverseStringGen)

  test("D12: longestLineLength with specific examples") {
    assertEquals(
      impl.longestLineLength(List("a", "abc", "ab", "abcd", "abc")),
      4,
    )
    assertEquals(impl.longestLineLength(List("aa", "bb", "cc", "dd")), 2)
    assertEquals(impl.longestLineLength(List("hello")), 5)
    assertEquals(impl.longestLineLength(List.empty[String]), 0)
    assertEquals(impl.longestLineLength(List("", "abc", "", "defg", "")), 4)
  }

  test("D12: longestLineLength with Unicode characters") {
    assertEquals(impl.longestLineLength(List("日本語", "hello", "世界")), 5)
    assertEquals(impl.longestLineLength(List("😊😊", "a")), 4)
  }

  test("D12: longestLineLength with very long strings") {
    val lines = List("a" * 10, "a" * 100, "a" * 1000)
    assertEquals(impl.longestLineLength(lines), 1000)
  }

  property(
    "D12: longestLineLength behaves like mapping to length and finding max",
  ) {
    forAll(diverseStringListGen) { lines =>
      if (lines.isEmpty) {
        impl.longestLineLength(lines) == 0
      } else {
        impl.longestLineLength(lines) == lines.map(_.length).max
      }
    }
  }

  property(
    "D12: longestLineLength is greater than or equal to any string length",
  ) {
    forAll(diverseStringListGen) { lines =>
      val maxLength = impl.longestLineLength(lines)
      lines.forall(s => s.length <= maxLength)
    }
  }

  property(
    "D12: longestLineLength equals at least one string length if list is non-empty",
  ) {
    forAll(nonEmptyDiverseStringListGen) { lines =>
      val maxLength = impl.longestLineLength(lines)
      lines.exists(s => s.length == maxLength)
    }
  }

  property("D12: longestLineLength of empty list returns 0") {
    impl.longestLineLength(List.empty[String]) == 0
  }

  property("D12: adding a longer string changes the maximum length") {
    forAll(diverseStringListGen, Gen.chooseNum(10, 100)) {
      (lines, extraLength) =>
        val originalMax = impl.longestLineLength(lines)
        val longerString = "a" * (originalMax + extraLength)

        impl.longestLineLength(longerString :: lines) == longerString.length
    }
  }

  property(
    "D12: longestLineLength of combined lists is the max of individual max lengths",
  ) {
    forAll(diverseStringListGen, diverseStringListGen) { (list1, list2) =>
      val combinedMax = impl.longestLineLength(list1 ++ list2)
      val max1 = if (list1.isEmpty) 0 else impl.longestLineLength(list1)
      val max2 = if (list2.isEmpty) 0 else impl.longestLineLength(list2)

      combinedMax == math.max(max1, max2)
    }
  }
}

class D13Suite extends ScalaCheckSuite {
  val impl: D13 = getImplementations().collect { case x: D13 => x }.head

  test("D13: elimEmptyLines removes empty strings") {
    assertEquals(
      impl.elimEmptyLines(List("", "abc", "", "def", "")),
      List("abc", "def"),
    )
  }

  test("D13: elimEmptyLines with no empty strings returns the original list") {
    val lines = List("a", "abc", "defg")
    assertEquals(impl.elimEmptyLines(lines), lines)
  }

  test("D13: elimEmptyLines with only empty strings returns an empty list") {
    assertEquals(
      impl.elimEmptyLines(List("", "", "")),
      List.empty[String],
    )
  }

  test("D13: elimEmptyLines with an empty list returns an empty list") {
    assertEquals(
      impl.elimEmptyLines(List.empty[String]),
      List.empty[String],
    )
  }

  test("D13: elimEmptyLines with whitespace strings") {
    assertEquals(
      impl.elimEmptyLines(List("", " ", "  ", "abc", "\t", "\n")),
      List("abc"),
    )
  }

  property("D13: result contains no empty strings") {
    forAll(diverseStringListGen) { lines =>
      impl.elimEmptyLines(lines).forall(_.nonEmpty)
    }
  }

  property("D13: result preserves the order of non-empty strings") {
    forAll(diverseStringListGen) { lines =>
      val nonEmptyLines = lines.filter(_.nonEmpty)
      impl.elimEmptyLines(lines) == nonEmptyLines
    }
  }

  property("D13: function is idempotent") {
    forAll(diverseStringListGen) { lines =>
      val once = impl.elimEmptyLines(lines)
      val twice = impl.elimEmptyLines(once)
      once == twice
    }
  }

  property("D13: result is a subset of the original list") {
    forAll(diverseStringListGen) { lines =>
      impl.elimEmptyLines(lines).forall(lines.contains)
    }
  }

  property(
    "D13: number of elements in result equals number of non-empty strings in original",
  ) {
    forAll(diverseStringListGen) { lines =>
      impl.elimEmptyLines(lines).length == lines.count(_.nonEmpty)
    }
  }

  property(
    "D13: concatenating results is the same as eliminating from concatenated input",
  ) {
    forAll(diverseStringListGen, diverseStringListGen) { (list1, list2) =>
      val result1 = impl.elimEmptyLines(list1)
      val result2 = impl.elimEmptyLines(list2)
      impl.elimEmptyLines(list1 ++ list2) == result1 ++ result2
    }
  }
}

class D14Suite extends ScalaCheckSuite {
  val impl: D14 = getImplementations().collect { case x: D14 => x }.head

  val nonEmptyDiverseStringListGen: Gen[List[String]] =
    Gen.nonEmptyListOf(diverseStringGen)

  test("D14: longestLine returns the longest string") {
    val lines = List("a", "abc", "ab", "abcd", "abc")
    assertEquals(impl.longestLine(lines), "abcd")
  }

  test(
    "D14: longestLine returns the last of multiple same-length longest strings",
  ) {
    val lines = List("abc", "def", "ghi", "jkl")
    assertEquals(impl.longestLine(lines), "jkl")
  }

  test("D14: longestLine with only one string returns that string") {
    val lines = List("hello")
    assertEquals(impl.longestLine(lines), "hello")
  }

  test("D14: longestLine handles empty strings in the list") {
    val lines = List("", "abc", "", "defg", "")
    assertEquals(impl.longestLine(lines), "defg")
  }

  test("D14: longestLine with only empty strings returns an empty string") {
    val lines = List("", "", "")
    assertEquals(impl.longestLine(lines), "")
  }

  property(
    "D14: returned string length equals maximum length in non-empty list",
  ) {
    forAll(nonEmptyDiverseStringListGen) { lines =>
      val result = impl.longestLine(lines)
      val maxLength = lines.map(_.length).max
      result.length == maxLength
    }
  }

  property("D14: returned string exists in the original list") {
    forAll(nonEmptyDiverseStringListGen) { lines =>
      lines.contains(impl.longestLine(lines))
    }
  }

  property("D14: if multiple max-length strings exist, returns the last one") {
    forAll(nonEmptyDiverseStringListGen.suchThat {
      _.map(_.length).distinct.size > 1
    }) { lines =>
      val maxLength = lines.map(_.length).max
      val allMaxLengthStrings = lines.filter(_.length == maxLength)

      impl.longestLine(lines) == allMaxLengthStrings.last
    }
  }
}

class D15Suite extends ScalaCheckSuite {
  val impl: D15 = getImplementations().collect { case x: D15 => x }.head

  test("D15: compress removes consecutive duplicates as shown in the example") {
    val input =
      List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    val expected = List('a', 'b', 'c', 'a', 'd', 'e')
    assertEquals(impl.compress(input), expected)
  }

  test("D15: compress preserves non-consecutive duplicates") {
    val input = List(1, 2, 3, 2, 1)
    val expected = List(1, 2, 3, 2, 1)
    assertEquals(impl.compress(input), expected)
  }

  test(
    "D15: compress with no consecutive duplicates returns the original list",
  ) {
    val input = List("a", "b", "c", "d")
    assertEquals(impl.compress(input), input)
  }

  test("D15: compress with all identical elements returns a single element") {
    val input = List(5, 5, 5, 5, 5)
    val expected = List(5)
    assertEquals(impl.compress(input), expected)
  }

  test("D15: compress with empty list returns empty list") {
    assertEquals(impl.compress(List.empty[Int]), List.empty[Int])
  }

  test("D15: compress with single element returns that element") {
    assertEquals(impl.compress(List("singleton")), List("singleton"))
  }

  test("D15: compress works with various patterns of repetition") {
    val input = List(1, 1, 2, 2, 2, 3, 4, 4, 5, 6, 6, 6, 6, 7, 7)
    val expected = List(1, 2, 3, 4, 5, 6, 7)
    assertEquals(impl.compress(input), expected)
  }

  test("D15: compress with alternating elements") {
    val input = List(1, 2, 1, 2, 1, 2)
    val expected = List(1, 2, 1, 2, 1, 2)
    assertEquals(impl.compress(input), expected)
  }

  test("D15: compress with custom case class elements") {
    case class Person(name: String, age: Int)

    val alice1 = Person("Alice", 30)
    val alice2 = Person("Alice", 30)
    val bob = Person("Bob", 25)

    val input = List(alice1, alice1, alice2, bob, bob)
    val expected = List(alice1, bob)
    assertEquals(impl.compress(input), expected)
  }

  property("D15: compress is idempotent") {
    forAll { (list: List[Int]) =>
      impl.compress(impl.compress(list)) == impl.compress(list)
    }
  }

  property("D15: compress produces a subsequence of the original list") {
    @tailrec
    def isSubsequence[A](sub: List[A], main: List[A]): Boolean = {
      if (sub.isEmpty) true
      else if (main.isEmpty) false
      else if (sub.head == main.head) isSubsequence(sub.tail, main.tail)
      else isSubsequence(sub, main.tail)
    }

    forAll { (list: List[Int]) =>
      isSubsequence(impl.compress(list), list)
    }
  }

  property(
    "D15: if no consecutive duplicates exist, compress returns the original list",
  ) {
    val genNoDuplicates = Gen.listOf(Gen.choose(1, 100)).map { xs =>
      if (xs.isEmpty) xs
      else xs.head :: xs.tail.zip(xs).collect { case (a, b) if a != b => a }
    }

    forAll(genNoDuplicates) { list =>
      impl.compress(list) == list
    }
  }

  property("D15: length of compress result is always <= original length") {
    forAll { (list: List[String]) =>
      impl.compress(list).length <= list.length
    }
  }
}

class D16Suite extends ScalaCheckSuite {
  val impl: D16 = getImplementations().collect { case x: D16 => x }.head

  test(
    "D16: averageOfDoubles calculates average correctly for the given example",
  ) {
    assertEquals(impl.averageOfDoubles(List(2.0, 2.5, 4.5)), 3.0)
  }

  test("D16: averageOfDoubles with single element returns that element") {
    assertEquals(impl.averageOfDoubles(List(5.0)), 5.0)
  }

  test("D16: averageOfDoubles with empty list should throw an exception") {
    intercept[IllegalArgumentException] {
      impl.averageOfDoubles(List.empty[Double])
    }
  }

  test("D16: averageOfDoubles with all same numbers returns that number") {
    assertEquals(impl.averageOfDoubles(List(3.0, 3.0, 3.0)), 3.0)
  }

  test("D16: averageOfDoubles with positive and negative numbers") {
    assertEquals(impl.averageOfDoubles(List(10.0, -10.0, 5.0, -5.0)), 0.0)
    assertEquals(impl.averageOfDoubles(List(-1.0, -3.0, -5.0)), -3.0)
  }

  test("D16: averageOfDoubles with floating point precision cases") {
    val result1 = impl.averageOfDoubles(List(1.0 / 3.0, 2.0 / 3.0, 3.0 / 3.0))
    assert(math.abs(result1 - 2.0 / 3.0) < 1e-15)

    val result2 = impl.averageOfDoubles(List(0.1, 0.2, 0.3))
    assert(math.abs(result2 - 0.2) < 1e-15)
  }

  test("D16: handles numerical stability with large and small values") {
    val mixedList = List(1e15, 1e15, 1e15, 1e-15, 1e-15, 1e-15)

    val expected = (3 * 1e15 + 3 * 1e-15) / 6
    val result = impl.averageOfDoubles(mixedList)

    val relativeError = math.abs((result - expected) / expected)
    assert(relativeError < 1e-10)
  }

  test("D16: averageOfDoubles with extreme values") {
    val smallValues = List(1e-100, 2e-100, 3e-100)
    assertEquals(impl.averageOfDoubles(smallValues), 2e-100)

    val largeValues = List(1e100, 2e100, 3e100)
    assertEquals(impl.averageOfDoubles(largeValues), 2e100)
  }

  val nonEmptyListOfDoubles = Gen.nonEmptyListOf(Gen.choose(-1000.0, 1000.0))

  property("D16: average is between min and max of the list") {
    forAll(nonEmptyListOfDoubles) { list =>
      val avg = impl.averageOfDoubles(list)
      val min = list.min
      val max = list.max

      min <= avg && avg <= max
    }
  }

  property(
    "D16: multiplying all elements by a constant multiplies the average",
  ) {
    forAll(nonEmptyListOfDoubles, Gen.choose(1.0, 10.0)) { (list, factor) =>
      val originalAvg = impl.averageOfDoubles(list)
      val scaledList = list.map(_ * factor)
      val scaledAvg = impl.averageOfDoubles(scaledList)

      math.abs(scaledAvg - originalAvg * factor) < 1e-10
    }
  }

  property(
    "D16: adding a constant to all elements adds that constant to the average",
  ) {
    forAll(nonEmptyListOfDoubles, Gen.choose(-100.0, 100.0)) {
      (list, constant) =>
        val originalAvg = impl.averageOfDoubles(list)
        val shiftedList = list.map(_ + constant)
        val shiftedAvg = impl.averageOfDoubles(shiftedList)

        math.abs(shiftedAvg - (originalAvg + constant)) < 1e-10
    }
  }

  property("D16: average of list of n identical elements is that element") {
    forAll(Gen.choose(-1000.0, 1000.0), Gen.choose(1, 100)) { (value, count) =>
      val list = List.fill(count)(value)
      math.abs(impl.averageOfDoubles(list) - value) < 1e-10
    }
  }

  property("D16: sum of deviations from the average is approximately zero") {
    forAll(nonEmptyListOfDoubles) { list =>
      val avg = impl.averageOfDoubles(list)
      val sumOfDeviations = list.map(_ - avg).sum

      math
        .abs(sumOfDeviations) < 1e-9 * list.size * math.max(1.0, math.abs(avg))
    }
  }

  property("D16: average is preserved when list order is shuffled") {
    forAll(nonEmptyListOfDoubles) { list =>
      val shuffled = scala.util.Random.shuffle(list)
      val originalAvg = impl.averageOfDoubles(list)
      val shuffledAvg = impl.averageOfDoubles(shuffled)

      math.abs(originalAvg - shuffledAvg) < 1e-10
    }
  }
}
