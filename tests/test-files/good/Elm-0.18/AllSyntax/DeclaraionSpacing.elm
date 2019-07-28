module Main exposing (annotationShouldBeGroupedWithItsDefinition, topLevelDestructingShouldNotGroupWithAnything, withADefinition, withAnAnnotation)

{-| Module comment.
-}


annotationShouldBeGroupedWithItsDefinition : ()
annotationShouldBeGroupedWithItsDefinition =
    ()


{-| Doc comment is grouped with the next thing
-}
withADefinition =
    ()


{-| Doc comment is grouped with the next thing
-}
withAnAnnotation : ()
withAnAnnotation =
    ()


( a, b ) =
    ( 1, 2 )


topLevelDestructingShouldNotGroupWithAnything =
    -- This declaration should not be grouped with the ( a, b ) declaration above
    5
