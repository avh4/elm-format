module Main exposing (banner)

import Nri.DEPRECATED.Button as Button


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
                , Button.buttonDeprecated
                    { label = "Sign up for FREE!"
                    , icon = Nothing
                    , size = Button.Large
                    , style = Button.Premium
                    , width = Just 280
                    , onClick = DropDownBanner.RecordSignupClick DropDownBanner.Home
                    , disabled = False
                    }
                ]
            , p [ class [ PremiumUpsellMessage ] ]
                [ Html.text "Interested in "
                , a [ Url.href (Url.fromString "/premium") ] [ Html.text "NoRedInk Premium" ]
                , Html.text "?"
                ]
            ]
