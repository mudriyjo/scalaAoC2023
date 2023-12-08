package AdventOfCode.Day7

class Day7Suite extends munit.FunSuite {
  test("Day7 test data") {
    val cardSum = 6440L
    val result = Day7.execute("_test.input")
    assertEquals(cardSum, result)
  }

  test("Day7 second test data") {
    val cardSum = 5905L
    val result = Day7second.execute("_2_test.input")
    assertEquals(cardSum, result)
  }
}
