module Main exposing (andThenExample1, andThenIn17, needsParens, onErrorExample1, qualifiedAndThen, tasksIn17, withComments)


andThenExample1 =
    a |> andThen b |> andThen c


qualifiedAndThen =
    a |> Result.andThen b |> Json.Decode.andThen c


onErrorExample1 =
    a |> andThen b |> onError c


withComments =
    a {- 1 -} |> andThen {- 2 -} b {- 3 -} |> andThen {- 4 -} c


needsParens =
    a 1 2 |> andThen (b 3 4) |> andThen (c 5 6)


andThenIn17 =
    String.toInt "1234"
        |> Result.andThen (\year -> isValidYear year)


tasksIn17 =
    Http.toTask (Http.getString "http://example.com/war-and-peace")
        |> Task.andThen (\fullText -> Task.succeed (NewText fullText))
        |> Task.onError (\error -> Task.succeed DidNotLoad)
