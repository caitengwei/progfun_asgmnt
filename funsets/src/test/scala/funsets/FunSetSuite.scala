package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val nature = (x: Int) => x >= 0
    val nonPos = (x: Int) => x <= 0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1 contains 1")
      assert(contains(s2, 2), "Singleton 2 contains 2")
      assert(contains(s3, 3), "Singleton 3 contains 3")
      
      assert(!contains(s1, 0), "Singleton 1 doesn't contains 0")
      assert(!contains(s2, 1), "Singleton 1 doesn't contains 1")
      assert(!contains(s3, 1000000), "Singleton 1 doesn't contains 1000000")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      
      val all = union(nature, nonPos)
      assert(contains(all, -10000), "All contains negtive No")
      assert(contains(all, 0), "All contains 0")
      assert(contains(all, 10000), "All contains positive No")
    }
  }
  
  test("intersect only contains common parts") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "s contains nothing")
      assert(!contains(s, 2), "s contains nothing")
      
      val zero = intersect(nature, nonPos)
      assert(!contains(zero, -10000), "Zero contains no negtive No")
      assert(contains(zero, 0), "Zero contains 0")
      assert(!contains(zero, 10000), "Zero contains no positive No")
    }
  }
  
  test("diff contains what's in s but not in t") {
    new TestSets {     
      val neg = diff(nonPos, nature)
      assert(contains(neg, -10000), "Neg contains negtive No")
      assert(!contains(neg, 0), "Neg contains no 0")
      assert(!contains(neg, 10000), "Neg contains no positive No")
    }
  }
  
  test("filter filtering elements from set") {
    new TestSets {
      val empty = filter(s1, (x: Int) => x != 1)
      assert(!contains(empty, 1), "Filter the only element")
      
      val filtered = filter(nature, (x: Int) => x < 10 || x > 100)
      assert(contains(filtered, 9), "9 is not filtered")
      assert(!contains(filtered, 10), "10 is filtered")
      assert(!contains(filtered, 100), "199 is filtered")
      assert(contains(filtered, 101), "101 is not filtered")
    }
  }
  
  test("forall checks if set match predicate") {
    new TestSets {
      assert(forall(nature, (x: Int) => x > -1 && x < 1001), "Should success 1")
      assert(!forall(nature, (x: Int) => x > 10), "Should fail 1")
      assert(!forall(nature, (x: Int) => x <= 999), "Should fail 2")
      assert(forall(nonPos, (x: Int) => x < 100), "Should success 2")
      assert(!forall(nonPos, (x: Int) => x < 0), "Should fail 3")
    }
  }
  
  test("exist checks if s has one integer in p") {
    new TestSets {
      assert(exists(nature, (x: Int) => x <= 0), "Should success 1")
      assert(exists(nature, (x: Int) => x > 999), "Should success 2")
      assert(!exists(nature, (x: Int) => x < 0), "Should fail 1")
    }
  }
  
  test("map transform set into another") {
    new TestSets {
      val new_set = map(nature, (x: Int) => x + 1)
      assert(!contains(new_set, 0), "0 isn't in new set")
      assert(contains(new_set, 1), "1 is in new set")
      assert(contains(new_set, 1000), "1000 is in new set")
      assert(!contains(new_set, -1000), "-1000 isn't in new set")
      
      val new_set_2 = map(s2, (x: Int) => x * x)
      assert(!contains(new_set_2, 2), "2 isn't in new set 2")
      assert(contains(new_set_2, 4), "4 is in new set 2")
    }
  }
}
