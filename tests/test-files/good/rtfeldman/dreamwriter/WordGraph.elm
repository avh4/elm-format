module Component.WordGraph exposing (Day, Entry, bar, barMargin, barWidth, graph, graphHeight, maxDays, scale, viewWordGraph)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Entry =
    { words : Int
    , day : String
    }


type alias Day =
    { amount : Float
    , day : String
    , xOffset : Int
    }


graphHeight =
    18


barWidth =
    10


barMargin =
    1


maxDays =
    14


bar : Day -> Float -> Svg
bar day yOffset =
    rect
        [ x <| toString <| day.xOffset * (barWidth + barMargin)
        , y <|
            toString <|
                if day.amount <= 0 then
                    yOffset

                else
                    yOffset - day.amount
        , height <| toString <| abs day.amount
        , width <| toString barWidth
        ]
        [ Svg.title [] [ text day.day ] ]


graph : Float -> List Day -> Svg
graph yOffset days =
    let
        graphWidth =
            List.length days * (barWidth + barMargin)

        axis =
            line
                [ x1 "0"
                , y1 <| toString yOffset
                , x2 <| toString (graphWidth - barMargin)
                , y2 <| toString yOffset
                ]
                []
    in
    svg
        [ id "doc-word-count-graph"
        , width <| toString graphWidth
        , height <| toString graphHeight
        ]
        [ g
            [ y <| toString yOffset
            ]
            {- SVG uses a painter algorithm, so we need axis at the end of
               the list to keep bars from overlapping the axis, which gets
               pretty ugly.
            -}
            (List.append (List.map (\day -> bar day yOffset) days)
                [ axis ]
            )
        ]


scale : Int -> Int -> Int -> Float
scale top bot value =
    let
        range =
            top - bot

        ratio =
            graphHeight
                / (if range == 0 then
                    0.1

                   else
                    toFloat range
                  )
    in
    ratio * toFloat value


viewWordGraph : List Entry -> Svg
viewWordGraph list =
    let
        lastTwoWeeks =
            List.take maxDays list

        max =
            List.map (\x -> x.words) lastTwoWeeks
                |> List.maximum
                |> Maybe.withDefault graphHeight

        min =
            List.map (\x -> x.words) lastTwoWeeks
                |> List.minimum
                |> Maybe.withDefault 0
                |> (\x ->
                        if x > 0 then
                            0

                        else
                            x
                   )

        yOffset =
            scale max min max

        days =
            List.map2
                (\offset entry ->
                    { amount = scale max min entry.words
                    , day = entry.day
                    , xOffset = offset
                    }
                )
                [0..maxDays - 1]
                lastTwoWeeks
    in
    graph yOffset days
