package macrotypeclass

import scala.annotation.targetName
import scala.quoted.{Expr, FromExpr, Quotes, Type, quotes}

object MacroTypeclass:

  def summonModule[M <: Singleton: Type](using Quotes): Option[M] =
    import quotes.reflect.*
    val name: String = TypeRepr.of[M].typeSymbol.companionModule.fullName
    val fixedName = name.replace(raw"$$.", raw"$$") + "$"
    try
      Option(Class.forName(fixedName).getField("MODULE$").get(null).asInstanceOf[M])
    catch
      case _: Throwable => None

trait FastSemigroup[A]:
  def combine(a1: Expr[A], a2: Expr[A])(using Quotes): Expr[A]

object FastSemigroup:
  inline def combine[A, MT <: FastSemigroup[A] & Singleton](inline a1: A, inline a2: A, inline MT: MT): A =
    ${ combineImpl[A, MT]('{ a1 }, '{ a2 }) }
  @targetName("combineImplicit")
  inline def combine[A, MT <: FastSemigroup[A] & Singleton](inline a1: A, inline a2: A)(using inline MT: MT): A =
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
