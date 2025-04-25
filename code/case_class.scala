package ncreep

import scala.Tuple.*
import scala.deriving.Mirror
import scala.compiletime.*
import scala.annotation.nowarn

/** An alternative to `Mirror` that makes it easier to work with fields and their types. */
trait CaseClass[A]:
  type Name <: String
  type Fields <: Tuple
  type Size <: Int

  inline def name: String = constValue[Name]

  inline def size: Int = constValue[Size]

object CaseClass:
  type Aux[A, Name0, Fields0, Size0] = CaseClass[A] {
    type Name = Name0
    type Fields = Fields0
    type Size = Size0
  }

  // for some reason the `Size` type can't be used in `constValue[ToString[Size]]`
  // in certain contexts if this is not marked as `inline`, no idea why...
  //
  // the warning below doesn't matter for presentation purposes
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given [A](
      using m: Mirror.ProductOf[A]): CaseClass.Aux[
    A,
    m.MirroredLabel,
    MakeCaseClass[m.MirroredElemLabels, m.MirroredElemTypes],
    Tuple.Size[m.MirroredElemLabels]
  ] = new CaseClass[A]:
    type Name = m.MirroredLabel
    type Fields = MakeCaseClass[m.MirroredElemLabels, m.MirroredElemTypes]
    type Size = Tuple.Size[m.MirroredElemLabels]

trait Field[Name <: String, Type]

type GetFieldName[F] <: String = F match
  case Field[name, ?] => name

type GetFieldType[F] = F match
  case Field[?, typ] => typ

type TupleToField[T] = T match
  case (name, typ) => Field[name, typ]

type MakeCaseClass[Names <: Tuple, Types <: Tuple] =
  Zip[Names, Types] `Map` TupleToField
