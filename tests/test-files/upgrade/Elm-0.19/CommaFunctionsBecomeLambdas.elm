module CommaFunctionsBecomeLabdas exposing (tuple2)

tuple2 = (,) 1 2
tuple2_partial = (,) 1
tuple2_function = (,)

tuple3 = (,,) 1 2 3
tuple3_partial1 = (,,) 1 2
tuple3_partial2 = (,,) 1
tuple3_function = (,,)

bad_tooManyArgs = (,,) 1 2 3 4 5

withComments = {-A-}(,,,){-B-}1{-C-}2
needsParens f = f ((,,) 1)
multiline_splitAll =
    (,,)
        { longRecordWithFields = 1, longSecondField = 2 }
        { longRecordWithFields = 3, longSecondField = 4 }
multiline_splitSome =
    (,,) { longRecordWithFields = 1, longSecondField = 2 }
        { longRecordWithFields = 3, longSecondField = 4 }
multiline_splitFirst =
    (,,,)
        { longRecordWithFields = 1, longSecondField = 2 }
multiline_fromComments =
    (,,) 1 --A
        2
notMultiline_joinFirst =
    (,,,) { longRecordWithFields = 1, longSecondField = 2 }
