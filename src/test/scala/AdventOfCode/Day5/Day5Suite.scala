package AdventOfCode.Day5

class Day5Suite extends munit.FunSuite {
  test("Day5 test data") {
    val minLocation = BigInt(35)
    val result = Day5.execute("_test.input")
    assertEquals(minLocation, result)
  }

  test("Day5 second test data") {
    val minLocation = 46L
    val result = Day5second.execute("_2_test.input")
    assertEquals(minLocation, result)
  }
}
