package AdventOfCode.Day2

class Day2Suite extends munit.FunSuite {
  test("Day2 test data") {
    val cubes = 2632
    val result = Day2.execute("_test.input")
    assertEquals(cubes, result)
  }

  test("Day2 second test data") {
    val cubes = 2286
    val result = Day2second.execute("_2_test.input")
    assertEquals(cubes, result)
  }
}
