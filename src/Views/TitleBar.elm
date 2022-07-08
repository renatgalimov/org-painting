module Views.TitleBar exposing (breadcrumb, widget)

import Css exposing (baseline, before, content, cursor, display, fontSize, height, inlineBlock, marginRight, padding, padding2, pointer, property, rem, verticalAlign, width)
import Html.Styled exposing (Attribute, Html, a, div, span, styled, text)
import Html.Styled.Attributes exposing (class, href)


logo : Html msg
logo =
    styled span
        [ marginRight (rem 1)
        , fontSize (rem 2)
        , padding (rem 1)
        ]
        [ class "title-bar-logo" ]
        [ text "Ink and Zett" ]


widget : List (Html msg) -> Html msg
widget breadcrumbs =
    styled div
        []
        [ class "title-bar" ]
        ([ logo
         , indexLink
         ]
            ++ breadcrumbs
        )


breadcrumbCss : Css.Style
breadcrumbCss =
    Css.batch
        [ marginRight (rem 1)
        , cursor pointer
        , before
            [ property "content" "url(\"../img/si-glyph-triangle-right.svg\")"
            , height (rem 1)
            , width (rem 1)
            , marginRight (rem 0.5)
            , verticalAlign baseline
            , display inlineBlock
            ]
        ]


breadcrumb : List (Attribute msg) -> List (Html msg) -> Html msg
breadcrumb =
    styled span [ breadcrumbCss ]


indexLink : Html msg
indexLink =
    breadcrumb
        [ class "breadcrumb" ]
        [ a [ href "index.html" ]
            [ text "Paintings"
            ]
        ]
