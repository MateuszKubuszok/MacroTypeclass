package macrotypeclass

import scala.quoted.{ Expr, FromExpr, quotes, Quotes, Type }

object MacroTypeclass:

  def summonModule[M <: Singleton: Type](using Quotes): Option[M] =
    import quotes.reflect.*
    val name: String = TypeRepr.of[M].typeSymbol.companionModule.fullName
    val fixedName = name.replace(raw"$$.", raw"$$") + "$"
    try
      Option(Class.forName(fixedName).getField("MODULE$").get(null).asInstanceOf[M])
    catch
      case _: Throwable => None

//  def testImpl[M: Type](expr: Expr[M])(using Quotes): Expr[String] = {
//    import quotes.reflect.*
//    val name: String = TypeRepr.of[M].typeSymbol.companionModule.fullName
//    val fixedName = name.replace(raw"$$.", raw"$$") + "$"
//    try {
//      val gimi = Class.forName(fixedName).getField("MODULE$").get(null).asInstanceOf[M]
//      Expr(gimi.toString)
//    } catch {
//      case _: Throwable => Expr(s"we failed for $fixedName")
//    }
//  }
//
//  inline def test[M <: Singleton](inline m: M): String = ${ testImpl('{ m }) }

trait FastSemigroup[A]:
  def combine(a1: Expr[A], a2: Expr[A])(using Quotes): Expr[A]

object FastSemigroup:
  inline def combine[A, MT <: FastSemigroup[A] & Singleton](inline a1: A, inline a2: A, inline MT: MT): A =
    ${ combineImpl[A, MT]('{ a1 }, '{ a2 }) }

  def combineImpl[A, MT <: FastSemigroup[A] & Singleton: Type](a1: Expr[A], a2: Expr[A])(using Quotes): Expr[A] =
    import quotes.*
    MacroTypeclass.summonModule[MT] match
      case Some(mt) => mt.combine(a1, a2)
      case None => reflect.report.throwError(s"${Type.show[MT]} cannot be used in macros")

object IntFastSemigroup extends FastSemigroup[Int]:
  def combine(a1: Expr[Int], a2: Expr[Int])(using Quotes): Expr[Int] = '{ ${ a1 } + ${ a2 } }

object Nested:
  object IntFastSemigroup extends FastSemigroup[Int]:
    def combine(a1: Expr[Int], a2: Expr[Int])(using Quotes): Expr[Int] = '{ ${ a1 } * ${ a2 } }
