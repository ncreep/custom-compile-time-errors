package ncreep

import scala.quoted._

// Inspired by
// https://github.com/tpolecat/typename

trait TypeName[A]:
  type Name

object TypeName:
  type Aux[A, Name0] = TypeName[A] { type Name = Name0 }

  transparent inline given [A]: TypeName[A] = ${ impl[A] }

  def impl[A](
      using t: Type[A],
      ctx: Quotes): Expr[TypeName[A]] =
    import quotes.reflect.*
    // inspired by
    // https://docs.scala-lang.org/scala3/guides/macros/quotes.html#type-variables-in-quoted-patterns
    Expr(Type.show[A]) match
      case '{ $str: tpe } =>
        '{
          new TypeName[A] {
            type Name = tpe
          }
        }

  /** Collecting the names of a tuple of types into a single tuple of names. 
    * This saves us the effort of converting between a tuple of of `TypeName` **values**
    * and a tuple **type** with all the names.
    */ 
  trait Tupled[T <: Tuple]:
    type Names <: Tuple

  object Tupled:
    type Aux[T <: Tuple, Names0] = Tupled[T] { type Names = Names0 }

  given Tupled.Aux[EmptyTuple, EmptyTuple] = new Tupled[EmptyTuple]:
    type Names = EmptyTuple

  given [H, T <: Tuple, HName, TNames <: Tuple](
      using hName: TypeName.Aux[H, HName],
      tNames: Tupled.Aux[T, TNames]): Tupled.Aux[H *: T, HName *: TNames] =
    new Tupled[H *: T]:
      type Names = HName *: TNames
