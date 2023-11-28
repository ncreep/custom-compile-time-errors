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

type CheckAllFields[C[_]] =
  Check[[Fields <: Tuple] =>> Fields FlatMap ApplyToField[C]]

type ApplyToField[C[_]] = [Field] =>> C[Field] Map RenderFieldError[Field]

type RenderFieldError[Field] = [Error] =>> "[" ++ GetFieldName[Field] ++ "]: " ++ Error

type ApplyCheck[Fields <: Tuple] = [C] =>> 
  C match
    case Check[check] => check[Fields]

type CheckAll[Fields <: Tuple, Checks <: Tuple] =
  Checks FlatMap ApplyCheck[Fields]

inline def performChecks[Checks <: Tuple, A](using caseClass: CaseClass[A]): Unit =
  type Errors = CheckAll[caseClass.Fields, Checks]

  inline erasedValue[Errors] match
    case _: EmptyTuple => ()
    case _: (h *: t) => error(constValue[RenderErrors[h *: t, caseClass.Name]])

type RenderErrors[Errors <: Tuple, TypeName <: String] =
  "Some checks failed for [" ++ TypeName ++ "]:\n" ++
    MakeLines[Itemize[Errors]]

type Itemize[Errors <: Tuple] =
  Errors Map ([Err] =>> "- " ++ Err)