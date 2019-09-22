module Main exposing (banner)

import ElmFix
import Nri.Ui.Button.V9 as Button


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
                    [ Button.large
                    , Button.exactWidth 280
                    , Button.onClick (DropDownBanner.RecordSignupClick DropDownBanner.Home)
                    , Button.premium
                    , ElmFix.remove
                    , ElmFix.remove
                    ]
                ]
            , p [ class [ PremiumUpsellMessage ] ]
                [ Html.text "Interested in "
                , a [ Url.href (Url.fromString "/premium") ] [ Html.text "NoRedInk Premium" ]
                , Html.text "?"
                ]
            ]
