package AdventOfCode.Day4

class Day4Suite extends munit.FunSuite {
  test("Day4 test data") {
    val cards = 13
    val result = Day4.execute("_test.input")
    assertEquals(cards, result)
  }

  test("Day4 second test data") {
    val scratchCards = 30L
    val result = Day4second.execute("_2_test.input")
    assertEquals(scratchCards, result)
  }
}
