module Views.Style exposing (..)

import Css exposing (fontWeight, height, hidden, int, margin, overflow, pct, zero)
import Css.Global exposing (body, children, each, global, html, typeSelector)
import Html.Styled exposing (Html)


mainContainers : Css.Style
mainContainers =
    Css.batch
        [ height (pct 100)
        , margin zero
        , fontWeight (int 300)
        ]


style : Html msg
style =
    global
        [ html
            [ overflow hidden, mainContainers ]
        , each [ html, body ] [ mainContainers ]
        , body [ children [ typeSelector "div" [ mainContainers ] ] ]
        ]
