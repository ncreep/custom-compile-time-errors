package ncreep

import ncreep.TypeName

@main def test(): Unit =
  type Checks =
    Check[ExactlyOneOf[Timestamp, "Timestamp"]] *:
      Check[HasAtMostFields[6]] *:
      CheckAllFields[StringFieldAtMost[20]] *:
      CheckAllFields[HasPrefix["some"]] *:
      EmptyTuple

  // sometimes you need to "recompile workspace" to get this properly working
  // performChecks[Checks, Foo]
end test
