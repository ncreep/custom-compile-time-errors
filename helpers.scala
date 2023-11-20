package ncreep

import scala.Tuple.*
import scala.compiletime.ops.int
import scala.compiletime.ops.string

type ??? = Nothing

// TODO used?
type Force[T <: Tuple] = T match
  case EmptyTuple => EmptyTuple
  case h *: t => h *: Force[t]

// a more loosely-typed version of `Tuple.*:`
type Prepend[X, Y] <: Tuple = (X, Y) match
  case (x, y) => x *: y

// a more loosely-typed version of `string.+`
type ++[A, B] <: String = (A, B) match
  case (a, b) => string.+[a, b]

// a more loosely-typed version of `int.+`
type +[A, B] = (A, B) match
  case (a, b) => int.+[a, b]

// a more loosely-typed version of `Tuple.FlatMap`
type FlatMap[Tup, F[_]] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => Concat[F[h], FlatMap[t, F]]

// a more loosely-typed version of `Tuple.Map`
type Map[Tup, F[_]] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => F[h] *: Map[t, F]

// a more loosely-typed version of `Tuple.size`
type Size[T] <: Int = T match
  case EmptyTuple => 0
  case x *: xs => 1 + Size[xs]

/** A helper to allow naming of type variables when matching types. */
type Is[A] = A

type Subtype[A, B] <: Boolean = A match
  case B => true
  case _ => false

type IfThenElse[Cond <: Boolean, IfTrue, IfFalse] = Cond match
  case true => IfTrue
  case false => IfFalse

// "unsafe" if running on an empty tuple
type MkString[T <: Tuple] =
  Fold[Init[T], Last[T], [A, B] =>> A ++ ", " ++ B]

type MakeLines[T <: Tuple] = Fold[T, "", [A, B] =>> A ++ "\n" ++ B]
