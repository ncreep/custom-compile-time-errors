package ncreep

import scala.Tuple.*
import scala.compiletime.ops.any.ToString
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int
import scala.compiletime.ops.int.{+ => _, ToString => _, *}
import scala.compiletime.ops.string.Matches

type CollectFieldsOfType[Type, Fields <: Tuple] =
  Fold[
    Fields,
    EmptyTuple,
    [Current, Acc] =>> IfThenElse[
      GetType[Current] Subtype Type,
      Current Prepend Acc,
      Acc
    ]
  ]

type ExactlyOneOf[Type, TypeName <: String] = [Fields <: Tuple] =>> CollectFieldsOfType[Type, Fields] match
  case EmptyTuple => Error["No [" ++ TypeName ++ "] field in class"]
  case h *: EmptyTuple => Okay
  case h *: t => Error[
      "Expected exactly one" ++ " [" ++ TypeName ++ "] " ++
        "field in class, but got " ++ ToString[Size[h *: t]] ++ ": " ++
        "[" ++ RenderFieldList[h *: t] ++ "]"
    ]

type RenderFieldList[Fields <: Tuple] =
  MkString[Fields Map GetName]

type HasAtMostFields[N <: Int] = [Fields <: Tuple] =>> Size[Fields] match
  case Is[n] =>
    (n <= N) OrFailWith
      "Too many fields in class: " ++ ToString[n] ++ " > " ++ ToString[N]

type HasPrefix[Prefix <: String] = [Field] =>> Matches[GetName[Field], Prefix ++ ".*"] OrFailWith
  "field is missing prefix [" ++ Prefix ++ "]"

type StringFieldAtMost[N <: Int] = [Field] =>> GetType[Field] match
  case SizedString[n] =>
    (n <= N) OrFailWith
      "string too large: " ++ ToString[n] ++ " > " ++ ToString[N]
  case String =>
    Error["unbounded strings are not allowed"]
  case _ => Okay
