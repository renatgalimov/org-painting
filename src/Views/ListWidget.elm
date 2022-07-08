module Views.ListWidget exposing (view)

import Css exposing (alignItems, displayFlex, flexDirection, flexStart, hidden, justifyContent, marginRight, maxHeight, minHeight, overflow, padding, pct, property, rem, row, start)
import Html.Styled exposing (Attribute, Html, div, img, styled)
import Html.Styled.Attributes exposing (class, src)
import Html.Styled.Lazy exposing (lazy2)
import Models.Image exposing (Image)
import Url.Builder


widget : List (Attribute msg) -> List (Html msg) -> Html msg
widget attributes =
    styled div
        [ displayFlex
        , flexDirection row
        , alignItems flexStart
        , overflow hidden
        ]
        (class "image-widget" :: attributes)


image : List (Attribute msg) -> List (Html msg) -> Html msg
image attributes =
    styled img
        [ padding (rem 1)
        , minHeight (pct 100)
        , property "object-fit" "contain"
        , property "object-position" "left"
        ]
        (class "image" :: attributes)


viewItem : String -> Image -> Html msg
viewItem directory targetImage =
    styled div
        [ displayFlex
        , justifyContent start
        , marginRight (rem 1)
        , maxHeight (pct 100)
        , property "align-content" "flex-start"
        ]
        [ class "image-widget-item" ]
        [ viewImage directory targetImage
        ]


{-| Display a painting widgets which displays all images side by side.
-}
view : String -> List Image -> Html msg
view directory images =
    widget
        []
        (List.map
            (viewItem directory)
            images
        )


viewImage : String -> Image -> Html msg
viewImage directory targetImage =
    image [ src (Url.Builder.relative [ directory, targetImage.path ] []) ] []
