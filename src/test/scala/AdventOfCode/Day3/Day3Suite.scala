package AdventOfCode.Day3

class Day3Suite extends munit.FunSuite {
  test("Day3 test data") {
    val schema = 4361
    val result = Day3.execute("_test.input")
    assertEquals(schema, result)
  }

  test("Day3 second test data") {
    val schema = 467835
    val result = Day3second.execute("_2_test.input")
    assertEquals(schema, result)
  }
}
