module Main (..) where


patternWithQualifiedConstructorAsCosntructorArgument m =
    case m of
        Maybe.Just (Maybe.Nothing) ->
            ()

        Maybe.Just _ ->
            ()

        Maybe.Nothing ->
            ()


patternWithUnqualifiedConstructorAsCosntructorArgument m =
    case m of
        Maybe.Just Nothing ->
            ()

        (Just _) as x ->
            ()

        Nothing as x ->
            ()

        ((Maybe.Nothing) as y) as x ->
            ()

        _ ->
            ()
