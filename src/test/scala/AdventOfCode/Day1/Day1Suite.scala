package AdventOfCode.Day1

class Day1Suite extends munit.FunSuite {
  test("Day1 test data") {
    val calibrationValue = List(12, 38, 15 ,77)
    val result = Day1.execute("_test.input")
    assertEquals(calibrationValue.sum, result)
  }

  test("Day1 second test data") {
    val calibrationValue = List(29, 83, 13, 24, 42, 14, 76)
    val result = Day1second.execute("_2_test.input")
    assertEquals(calibrationValue.sum, result)
  }
}
