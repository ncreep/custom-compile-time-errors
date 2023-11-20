package ncreep

import scala.compiletime.*

/** A wrapper for checks to be applied to a tuple of fields. This is a workaround for the fact that we cannot hold
  * type-constructors in a tuple, but must have a fully saturated type instead.
  */
trait Check[C[_ <: Tuple]]

type Error[Message <: String] = Message *: EmptyTuple

type Okay = EmptyTuple

type OrFailWith[Cond <: Boolean, ErrorMessage <: String] = 
  IfThenElse[Cond, Okay, Error[ErrorMessage]]

type ApplyCheck[Fields] = [C] =>> C match
  case Check[check] => check[Fields]

type CheckAll[Fields <: Tuple, Checks <: Tuple] =
  Checks FlatMap ApplyCheck[Fields]

type CheckAllFields[C[_]] =
  Check[[Fields <: Tuple] =>> Fields FlatMap ApplyToField[C]]

type RenderFieldError[Field] = [Error] =>> "[" ++ GetName[Field] ++ "]: " ++ Error

type ApplyToField[Check[_]] = [Field] =>> Check[Field] Map RenderFieldError[Field]

inline def performChecks[Checks <: Tuple, A](using fields: CaseClassFields[A]): Unit =
  type Errors = CheckAll[fields.Values, Checks]

  inline erasedValue[Errors] match
    case _: EmptyTuple => ()
    case _: (h *: t) => error(constValue[RenderErrors[h *: t, fields.Name]])

type RenderErrors[Errors <: Tuple, TypeName <: String] =
  "Some checks failed for [" ++ TypeName ++ "]:\n" ++
    MakeLines[Itemize[Errors]]

type Itemize[Errors <: Tuple] =
  Errors Map ([Err] =>> "- " ++ Err)