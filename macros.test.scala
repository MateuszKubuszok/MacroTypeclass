//> using lib "org.scalameta::munit::0.7.27"

package macrotypeclass

class MacroTypeclassTest extends munit.FunSuite {

  test("explicit passage of a macro type class works") {
    assert(FastSemigroup.combine(1, 2, IntFastSemigroup) == 3)
    assert(FastSemigroup.combine(1, 2, Nested.IntFastSemigroup) == 2)
  }

  test("implicit passage of a macro type class works") {
    locally {
      implicit def fs: IntFastSemigroup.type = ??? // only type matters
      assert(FastSemigroup.combine(1, 2) == 3)
    }
    locally {
      implicit def fs: Nested.IntFastSemigroup.type = ??? // only type matters
      assert(FastSemigroup.combine(1, 2) == 2)
    }
  }
}
