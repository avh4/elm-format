

addParensToAnd =
    1 == 2 && 3 /= 4 && 5 < 6 && 7 > 8 && 9 <= 10 && 11 >= 12

addParensToAndWithComplexConditions =
    1 == 2 + 3 && 4 /= 5 && 6 < 7 + 8 && 9 > 10 && 11 <= 12 && 13 >= 14 + 15

addParensToAndMultiline =
    1
    == 2 && 3 /= 4 && 5 < 6 && 7 > 8 && 9 <= 10 && 11 >= 12

makesGroupedExpressionsSingleLine =
  1
    ==
      2
    +
      7
    *
      5
    -
      6
    +
      -8
    &&
      3
    /=
      4


doesntAddParensAroundParens =
  (1 == 2) && (3 /= 4) && 5/= 6 && (7 /= 8)

keepsNewLinesInExistingParens =
  (1
    == 2
    + 3 * 4 - 5 + 6) && 7 /= 8
