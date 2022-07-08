module Views.ImageWidget exposing (..)

import Color exposing (Color)
import Color.Manipulate
import Css exposing (alignItems, auto, column, cursor, displayFlex, ellipsis, flex2, flex3, flexBasis, flexDirection, flexGrow, flexShrink, flexStart, flexWrap, height, hex, hidden, int, lineHeight, marginBottom, minHeight, noWrap, overflow, padding, pct, pointer, property, pt, rem, row, textOverflow, whiteSpace, wrap, zero)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (Attribute, Html, button, div, img, param, styled)
import Html.Styled.Attributes exposing (class)


type Type
    = SideBySide
    | Single


view : String -> Html msg
view source =
    styled div [] [] []


widget : List (Attribute msg) -> List (Html msg) -> Html msg
widget attributes =
    styled div
        [ displayFlex
        , flexDirection row
        , alignItems flexStart
        , overflow hidden
        ]
        ([ class "image-widget" ] ++ attributes)


image : List (Attribute msg) -> List (Html msg) -> Html msg
image attributes =
    styled img
        [ padding (rem 1)
        , minHeight (pct 100)
        , property "object-fit" "contain"
        , property "object-position" "left"
        ]
        ([ class "image" ] ++ attributes)
