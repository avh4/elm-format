module ConvertBasicsRem exposing (fullyApplied)


fullyApplied =
    remainderBy 4 -1


fullyApplied_comments =
    remainderBy {- B -} 4 {- A -} -1


partiallyApplied =
    \divisor -> remainderBy divisor 1


unapplied =
    \dividend divisor -> remainderBy divisor dividend


qualified =
    remainderBy 4 11


extraArgs =
    remainderBy 2 1 3 4
