//> using lib "org.scalameta::munit::0.7.27"

package macrotypeclass

class MacroTypeclassTest extends munit.FunSuite {

  test("test") {
    println(FastSemigroup.combine(1, 2, IntFastSemigroup))
    println(FastSemigroup.combine(1, 2, Nested.IntFastSemigroup))
  }
}
