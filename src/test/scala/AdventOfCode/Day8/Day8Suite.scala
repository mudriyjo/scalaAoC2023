package AdventOfCode.Day8

class Day8Suite extends munit.FunSuite {
  test("Day8 test data") {
    val moveNum = 2L
    val result = Day8.execute("_test.input")
    assertEquals(moveNum, result)
  }

  test("Day8 second test data") {
    val moveNum = 6L
    val result = Day8second.execute("_2_test.input")
    assertEquals(moveNum, result)
  }
}
