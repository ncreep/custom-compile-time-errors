package ncreep.implicits

import ncreep.*
import scala.annotation.implicitNotFound

class ExactlyOneTimestamp[A]

object ExactlyOneTimestamp:
  given [A](
      using fields: CaseClassFields[A])(
      using Size[CollectFieldsOfType[Timestamp, fields.Values]] =:= 1): ExactlyOneTimestamp[A] =
    ExactlyOneTimestamp[A]()

def checkTimestampFields[A](
    using 
    @implicitNotFound("The [Timestamp] did not appear exactly once in ${A}") 
    exactlyOnce: ExactlyOneTimestamp[A]): Unit = ()

case class Bar(t: Timestamp, t2: Timestamp)
@main def test(): Unit =
  checkTimestampFields[Bar]
  ???
end test
