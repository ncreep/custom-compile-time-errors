package ncreep.implicits

import ncreep.*

import scala.annotation.implicitNotFound

// Reimplementing the functionality with `error` using `implicitNotFound`
trait PerformChecks[Checks, A]:
  type Errors

object PerformChecks:
  type Aux[Checks, A, Errors0] = PerformChecks[Checks, A] { type Errors = Errors0 }

  given [Checks <: Tuple, A, Errors0](
      using caseClass: CaseClass[A],
      ev: Errors0 =:= RenderErrors[CheckAll[caseClass.Fields, Checks], caseClass.Name])
      : PerformChecks.Aux[Checks, A, Errors0] =
    new PerformChecks[Checks, A]:
      type Errors = Errors0

// Currying type-parameters
// https://tpolecat.github.io/2015/07/30/infer.html
class CheckHelper[Checks, A]:
  def perform[Errors](
      using performChecks: PerformChecks[Checks, A])(
      // using a separate argument list so that the free `Errors`
      // type parameter gets fixed to the actual type in `PerformChecks`
      using Errors =:= performChecks.Errors)(
      // if `Errors` is not empty, this message will trigger
      using @implicitNotFound("${Errors}")
      ev: Errors =:= EmptyTuple): Unit = ()

def check[Checks, A] = CheckHelper[Checks, A]()

@main def test(): Unit =
  // kind of works, except that it doesn't render newlines correctly
  // check[SchemaOkay, HotelTable].perform

  ()
end test
