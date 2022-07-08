module Views.Sidebar exposing (..)

import Color exposing (Color)
import Css exposing (column, cursor, ellipsis, flexGrow, height, hex, hidden, lineHeight, marginBottom, noWrap, overflow, padding, pct, pointer, rem, textOverflow, whiteSpace, zero)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (Attribute, Html, button, div, styled, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)


type alias Theme =
    { primary : Color
    }



-- /* SCSS RGB */
-- $snow: rgba(255, 251, 254, 1);
-- $gray-web: rgba(122, 125, 125, 1);
-- $light-gray: rgba(208, 207, 207, 1);
-- $davys-grey: rgba(86, 82, 84, 1);
-- $white: rgba(255, 255, 255, 1);
-- /* SCSS RGB */
-- $eerie-black: rgba(28, 28, 28, 1);
-- $gainsboro: rgba(218, 221, 216, 1);
-- $alabaster: rgba(236, 235, 228, 1);
-- $cultured: rgba(238, 240, 242, 1);
-- $ghost-white: rgba(250, 250, 255, 1);


viewSelectWidget : msg -> Html msg
viewSelectWidget nextWidget =
    styled div
        []
        [ class "widget-selector" ]
        [ styled button [  ] [ onClick nextWidget ] [ text "Widget" ]
        ]


defaultTheme : Theme
defaultTheme =
    { primary = Color.rgb255 238 240 242
    }



-- background: darken($color-widget-background, 10%)
-- width: 8rem
-- padding: 1rem 1rem
-- height: 100%
-- transition: 0.3s


toCss : Color -> Css.Color
toCss color =
    let
        hsla =
            Color.toHsla color
    in
    Css.hsla hsla.hue hsla.saturation hsla.lightness hsla.alpha


sidebarBaseCss : Css.Style
sidebarBaseCss =
    Css.batch
        [ Css.displayFlex
        , Css.flexDirection
            column
        , Css.backgroundColor (defaultTheme.primary |> toCss)
        , Css.height (pct 100)
        , Css.padding2 (rem 1) (rem 1)
        , transition [ Transitions.width 300 ]
        ]


sidebarItemBaseCss : Css.Style
sidebarItemBaseCss =
    Css.batch
        [ overflow hidden
        , textOverflow
            ellipsis
        , whiteSpace noWrap
        , flexGrow zero
        , marginBottom
            (rem 1)
        , height (rem 5)
        , padding (rem 1)
        , cursor pointer
        , lineHeight (rem 1)
        ]


todo : List (Attribute msg) -> List (Html msg) -> Html msg
todo =
    styled div
        [ sidebarItemBaseCss ]


sidebar : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
sidebar expanded attributes =
    styled div
        [ sidebarBaseCss
        , if expanded then
            Css.width (pct 50)

          else
            Css.width (rem 8)
        ]
        (attributes ++ [ class "sidebar" ])


topLevelLink : List (Attribute msg) -> List (Html msg) -> Html msg
topLevelLink =
    styled div [ sidebarItemBaseCss ]


toggle : List (Attribute msg) -> List (Html msg) -> Html msg
toggle =
    styled button []
