package ncreep

import scala.Tuple.{++ => _, *}
import scala.compiletime.ops.any.ToString
import scala.compiletime.ops.int.{ToString => _, *}
import scala.compiletime.ops.string.Matches

// format: off

type SchemaOkay =
  HasAtMostFields[25] *:
  ExactlyOneOf[Key, "Key"] *:
  ExactlyOneOf[Timestamp, "Timestamp"] *:
  StringFieldsAtMost[500] *:
  HasPrefix["tr"] *:
  EmptyTuple

type HasAtMostFields[MaxSize <: Int] = Check[
  [Fields <: Tuple] =>> 
    // no way to name intermediate results without matching
    // so we have to repeat the call to `Size` twice (or we can do a match instead)
    (Size[Fields] <= MaxSize) `OrFailWith` 
      "Too many fields in class: " ++ ToString[Size[Fields]] ++ " > " ++ ToString[MaxSize]
]

/* this trick doesn't work on Scala 3.4 */

// same as `HasAtMostFields` but without calling `Size` twice, instead matching on the
// result of `Size` to assign it to a value
// type HasAtMostFields2[MaxSize <: Int] = Check[
//   [Fields <: Tuple] =>> 
//     Size[Fields] match
//       case Is[size] =>
//         (size <= MaxSize) OrFailWith 
//           "Too many fields in class: " ++ ToString[size] ++ " > " ++ ToString[MaxSize]
// ]

type ExactlyOneOf[Type, TypeName <: String] = Check[
  [Fields <: Tuple] =>> 
    CollectFieldsOfType[Type, Fields] match
      case EmptyTuple => Error["No [" ++ TypeName ++ "] field in class"]
      case h *: EmptyTuple => Okay
      case h *: t => Error[
        "Expected exactly one [" ++ TypeName ++ "] " ++
          "field in class, but got " ++ ToString[Size[h *: t]] ++ ": " ++
          "[" ++ RenderFieldList[h *: t] ++ "]"
     ]
]

type CollectFieldsOfType[Type, Fields <: Tuple] =
 Fields `Filter` ([Field] =>> GetFieldType[Field] `Subtype` Type)

type RenderFieldList[Fields <: Tuple] =
 MkString[Fields `Map` GetFieldName]

type StringFieldsAtMost[MaxSize <: Int] = CheckAllFields [
 [Field] =>> 
   GetFieldType[Field] match
     case SizedString[n] =>
       (n <= MaxSize) `OrFailWith`
         "string is too large: " ++ ToString[n] ++ " > " ++ ToString[MaxSize]
     case String =>
       Error["unbounded strings are not allowed"]
     case _ => Okay
]

type HasPrefix[Prefix <: String] = CheckAllFields[
  [Field] =>> 
    Matches[GetFieldName[Field], Prefix ++ ".*"] `OrFailWith`
      "field is missing prefix [" ++ Prefix ++ "]"
]

// format: on
