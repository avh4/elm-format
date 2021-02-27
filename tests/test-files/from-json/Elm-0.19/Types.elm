module Declarations exposing (MyFunctionType, MyRecordType, MyRecordTypeExtension, MyTupleType, MyTypeReference, MyTypeReferenceWithArguments, MyTypeVariable, MyUnit)


type alias MyUnit =
    ()


type alias MyTypeReference =
    Basics.String


type alias MyTypeReferenceWithArguments =
    Result.Result () ()


type alias MyTypeVariable a =
    a


type alias MyTupleType =
    ( (), () )


type alias MyRecordType =
    { x : ()
    , y : ()
    }


type alias MyRecordTypeExtension r =
    { r
        | x : ()
        , y : ()
    }


type alias MyFunctionType =
    () -> () -> ()
