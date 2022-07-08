module Views.OverlayWidget exposing (..)

import Css exposing (absolute, backgroundColor, fixed, height, left, maxHeight, maxWidth, pct, position, right, top, width, zero)
import Html.Styled exposing (Attribute, Html, button, div, img, styled, text)
import Html.Styled.Attributes exposing (class, src)
import Html.Styled.Events exposing (onMouseDown, onMouseUp)
import List.Extra
import Models.Image exposing (Image)


type alias OverlayImageList =
    List Image


type alias OverlayModel =
    { left : Image
    , right : Image
    , toggled : Bool
    }


newOverlayImageList : List Image -> OverlayImageList
newOverlayImageList images =
    List.Extra.uniqueBy (\image -> image.path) images


newModel : List Image -> Maybe OverlayModel
newModel images =
    case images of
        left :: right :: _ ->
            Just
                { left = left
                , right = right
                , toggled = False
                }

        _ ->
            Nothing


{-| Find an item matching a predicate, and return it with a list of all other items.
-}
popImage : Image -> List Image -> Maybe (List Image)
popImage image list =
    List.Extra.findIndex (\item -> item.path == image.path) list
        |> Maybe.map (\index -> List.Extra.removeAt index list)


{-| Find two images in a list that match a predicate, and return them with a list of all other items.
-}
findTwo : Image -> Image -> List Image -> ( Maybe Image, Maybe Image, List Image )
findTwo leftImage rightImage list =
    let
        ( maybeLeft, remaining ) =
            popImage leftImage list
                |> Maybe.map (\remainingImages -> ( Just leftImage, remainingImages ))
                |> Maybe.withDefault ( Nothing, list )

        ( maybeRight, remainingʹ ) =
            popImage rightImage remaining
                |> Maybe.map (\remainingImages -> ( Just rightImage, remainingImages ))
                |> Maybe.withDefault ( Nothing, remaining )
    in
    ( maybeLeft, maybeRight, remainingʹ )


{-| Update left and right according to a list of given images.

If images exist in the list, re-use them, otherwise select appropriate
ones from the given list.

-}
withImages : List Image -> OverlayModel -> Maybe OverlayModel
withImages images model =
    let
        ( maybeLeft, maybeRight, remaining ) =
            findTwo model.left model.right images

        leftAndRight : Maybe ( Image, Image )
        leftAndRight =
            case ( maybeLeft, maybeRight ) of
                ( Just left, Just right ) ->
                    Just ( left, right )

                ( Nothing, Just right ) ->
                    case remaining of
                        first :: _ ->
                            Just ( first, right )

                        _ ->
                            Nothing

                ( Just left, Nothing ) ->
                    case remaining of
                        first :: _ ->
                            Just ( left, first )

                        _ ->
                            Nothing

                ( Nothing, Nothing ) ->
                    case remaining of
                        first :: second :: _ ->
                            Just ( first, second )

                        _ ->
                            Nothing
    in
    leftAndRight |> Maybe.map (\( left, right ) -> { model | left = left, right = right })


widget : List (Attribute msg) -> List (Html msg) -> Html msg
widget attributes =
    styled div
        [ backgroundColor (Css.hex "#151617")
        , position absolute
        , left zero
        , top zero
        , width (pct 100)
        , height (pct 100)
        ]
        (class "overlay-widget" :: attributes)


toggleButton : Html msg
toggleButton =
    styled button [] [] [ text "Toggle" ]


controlBar : Html msg
controlBar =
    styled div
        [ position fixed
        ]
        []
        []


imgCss : Css.Style
imgCss =
    Css.batch
        [ maxWidth (pct 100)
        , maxHeight (pct 100)
        , position absolute
        , left zero
        , top zero
        ]


type Msg
    = Toggled Bool


view : String -> OverlayModel -> (Msg -> msg) -> Html msg
view directory model wrapper =
    let
        top =
            if model.toggled then
                model.right

            else
                model.left

        bottom =
            if model.toggled then
                model.left

            else
                model.right
    in
    widget
        [ onMouseDown (wrapper (Toggled True))
        , onMouseUp (wrapper (Toggled False))
        ]
        [ styled img
            [ imgCss ]
            [ src (directory ++ "/" ++ top.path) ]
            []
        , styled img
            [ imgCss
            ]
            [ src (directory ++ "/" ++ bottom.path) ]
            []
        ]
