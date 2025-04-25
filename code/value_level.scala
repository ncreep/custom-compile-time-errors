package ncreep.value_level

import ncreep.*

import scala.Tuple.{++ => _, *}
import scala.compiletime.*
import scala.compiletime.ops.any.ToString

object ValueLevel:
  def fold[A, B](ls: List[A], init: B, f: (A, B) => B): B = ls.foldRight(init)(f)

  def toString(value: Any) = value.toString

  def nums = 1 :: 2 :: 3 :: 4 :: Nil

  def map[A, B](ls: List[A], f: A => B): List[B] =
    ls match
      case Nil => Nil
      case h :: t => f(h) :: map(t, f)

  def mapped = nums.map(i => i + 10)

  def filtered = mapped.filter(i => i < 14)

  def result = fold(filtered, "", (i, acc) => toString(i) ++ acc)

end ValueLevel

object TypeLevel:
  type Nums = 1 *: 2 *: 3 *: 4 *: EmptyTuple

  type Map[T <: Tuple, F[_]] <: Tuple =
    T match
      case EmptyTuple => EmptyTuple
      case h *: t => F[h] *: Map[t, F]

  type Mapped = Nums `Map` ([I] =>> I + 10)

  type Filtered = Mapped `Filter` ([I] =>> I < 14)

  type Result = Fold[Filtered, "", ([I, Acc] =>> ToString[I] ++ Acc)]

end TypeLevel

@main def test(): Unit =
  println(ValueLevel.result)

  println(constValue[TypeLevel.Result])
