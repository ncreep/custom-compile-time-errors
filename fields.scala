package ncreep

import scala.Tuple.*
import scala.deriving.Mirror

// TODO using TypeName?
trait Field[Name <: String, Type, TypeName <: String]

type GetName[F] <: String = F match
  case Field[name, ?, ?] => name

type GetType[F] = F match
  case Field[?, typ, ?] => typ

type GetTypeName[F] = F match
  case Field[?, ?, typeName] => typeName

type TupleToField[T] = T match
  case ((name, typ), typeName) => Field[name, typ, typeName]

type MakeFields[Names <: Tuple, Types <: Tuple, TypeNames <: Tuple] =
  Zip[Zip[Names, Types], TypeNames] Map TupleToField

/** An alternative to `Mirror` that makes it easier to work with fields and their types. */
trait CaseClassFields[A]:
  type Name <: String
  type Values <: Tuple

object CaseClassFields:
  type Aux[A, Name0, Values0] = CaseClassFields[A] { type Name = Name0; type Values = Values0 }

  given [A](
      using m: Mirror.ProductOf[A],
      // TODO used?
      tn: TypeName.Tupled[m.MirroredElemTypes]): CaseClassFields.Aux[A, m.MirroredLabel, MakeFields[
    m.MirroredElemLabels,
    m.MirroredElemTypes,
    tn.Names
  ]] =
    new CaseClassFields[A]:
      type Name = m.MirroredLabel
      type Values = MakeFields[m.MirroredElemLabels, m.MirroredElemTypes, tn.Names]
