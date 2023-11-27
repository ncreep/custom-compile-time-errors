package ncreep

import scala.compiletime.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.int.>

inline def checkSize1[A](maxSize: Int)(
    using caseClass: CaseClass[A]): Unit =
  inline if caseClass.size > maxSize then
    error("Too many fields in class")

// doesn't actually work
inline def checkSize2[A](maxSize: Int)(
    using caseClass: CaseClass[A]): Unit =
  inline if caseClass.size > maxSize then
    error(
      "Too many fields in class:" +
        caseClass.size + " > " +  maxSize)

inline def checkSize3[A, MaxSize <: Int](
    using caseClass: CaseClass[A]): Unit =
  inline if 
    constValue[caseClass.Size] > constValue[MaxSize] 
  then
      error(
        "Too many fields in class: " +
          constValue[ToString[caseClass.Size]] +
          " > " +
          constValue[ToString[MaxSize]])

inline def checkSize4[A, MaxSize <: Int](
    using caseClass: CaseClass[A]): Unit =
  inline if erasedValue[caseClass.Size > MaxSize] then
      type Message =
        "Too many fields in class: " ++
          ToString[caseClass.Size] ++
          " > " ++
          ToString[MaxSize]

      error(constValue[Message])

@main def testSimpleChecks(): Unit =
  // checkSize1[HotelTable](3)
  // checkSize2[HotelTable](3)
  // checkSize3[HotelTable, 3]
  // checkSize4[HotelTable, 3]

  ()
end testSimpleChecks
