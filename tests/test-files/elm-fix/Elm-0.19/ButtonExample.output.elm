module Main exposing (banner)


banner : Html Msg
banner =
    Html.map DropDownBannerMsg <|
        Banner.view
            { maybeBackgroundImage = Just Nri.Assets.marketingSite_homeBannerPhoto_png
            , backgroundColor = azure
            , bannerHeight = Banner.Tall
            }
            [ div [ class [ BannerContent ] ]
                [ h1 [ class [ BannerHeader ] ] [ Html.text "Unleash the writer within." ]
                , p [ class [ BannerText ] ] [ Html.text "NoRedInk builds stronger writers through interest-based curriculum, adaptive exercises, and actionable data." ]
                , Button.button "Sign up for FREE!"
                    [ case Button.Large of
                        Button.Large ->
                            Button.large

                        _ ->
                            ElmFix.todo "handle upgrading other sizes"
                    , case Just 280 of
                        Just w ->
                            Button.exactWidth w

                        Nothing ->
                            ElmFix.remove
                    , Button.onClick (DropDownBanner.RecordSignupClick DropDownBanner.Home)
                    , case Button.Premium of
                        Button.Premium ->
                            Button.premium

                        Button.Normal ->
                            ElmFix.remove

                        _ ->
                            ElmFix.todo "Handle other styles"
                    , case Nothing of
                        Nothing ->
                            ElmFix.remove

                        Just i ->
                            Button.icon i
                    , if False then
                        Button.disabled

                      else
                        ElmFix.remove
                    ]
                ]
            , p [ class [ PremiumUpsellMessage ] ]
                [ Html.text "Interested in "
                , a [ Url.href (Url.fromString "/premium") ] [ Html.text "NoRedInk Premium" ]
                , Html.text "?"
                ]
            ]
