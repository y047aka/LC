module View exposing (links, repositories, siteFooter, siteHeader)

import Html exposing (..)
import Html.Attributes exposing (..)


siteHeader : Html msg
siteHeader =
    Html.header [ class "site-header" ]
        [ h1 [] [ text "Space Launch Calendar 2019" ]
        ]


links : Html msg
links =
    section []
        [ h1 [] [ text "Links" ] ]


repositories : Html msg
repositories =
    section []
        [ h1 [] [ text "Repositories" ] ]


siteFooter : Html msg
siteFooter =
    footer [ class "site-footer" ]
        [ p [ class "copyright" ]
            [ text "© 2019 "
            , a [ href "https://sorabatake.jp", target "_blank" ] [ text "sorabatake" ]
            ]
        ]
