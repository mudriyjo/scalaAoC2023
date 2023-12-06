package AdventOfCode.Day6

class Day6Suite extends munit.FunSuite {
  test("Day6 test data") {
    val distanceCount = 288
    val result = Day6.execute("_test.input")
    assertEquals(distanceCount, result)
  }

  test("Day6 second test data") {
    val distanceCount = 71503
    val result = Day6second.execute("_2_test.input")
    assertEquals(distanceCount, result)
  }
}
