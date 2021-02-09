module RemoveUnnecessaryCommas exposing (leadingCommaList)


leadingCommaList =
    [ {- A -} {- B -} 1
    , 2
    ]


leadingCommaRecord =
    { {- A -} {- B -} x {- C -} = 1
    , y = 2
    }


leadingCommaRecordExtension =
    { base
        | {- A -} {- B -} x {- C -} = 1
        , y = 2
    }


type alias LeadingCommaRecordType =
    { {- A -} {- B -} x {- C -} : Int
    , y : Int
    }


type alias LeadingCommaRecordExtensionType base =
    { base
        | {- A -} {- B -} x {- C -} : Int
        , y : Int
    }
