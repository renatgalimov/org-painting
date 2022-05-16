module Projects exposing (main)

import Html exposing (Html, a, table, td, text, tr)
import Html.Attributes exposing (href)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, field, list, string)
import TimeTravel.Browser as TimeTravel exposing (defaultConfig)
import Url.Builder



-- CONSTANTS


paintingUrl : String
paintingUrl =
    "painting.html"


main =
    TimeTravel.element Debug.toString
        Debug.toString
        defaultConfig
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- INIT


init : String -> ( Model, Cmd Msg )
init _ =
    ( { paintings = [] }
    , getDirectory
    )



-- VIEW


viewPaintingLink : PaintingLink -> Html msg
viewPaintingLink paintingLink =
    let
        fullPantingLink =
            Url.Builder.absolute [ paintingUrl ] [ Url.Builder.string "json" paintingLink.location ]
    in
    tr []
        [ td [] [ a [ href fullPantingLink ] [ text paintingLink.title ] ]
        ]


view : Model -> Html msg
view model =
    table []
        (List.map
            viewPaintingLink
            model.paintings
        )


type alias PaintingLink =
    { title : String
    , location : String
    }


type alias Directory =
    { paintings : List PaintingLink
    }


type alias Model =
    { paintings : List PaintingLink
    }


type Msg
    = GotDirectory (Result Http.Error Directory)


directoryDecoder : Decoder Directory
directoryDecoder =
    Json.Decode.map Directory (field "paintings" (list paintingLinkDecoder))


directoryFile : String
directoryFile =
    "index.json"


getDirectory : Cmd Msg
getDirectory =
    Http.get
        { url = directoryFile
        , expect = expectJson GotDirectory directoryDecoder
        }


paintingLinkDecoder : Decoder PaintingLink
paintingLinkDecoder =
    Json.Decode.map2 PaintingLink
        (field "title" string)
        (field "path" string)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDirectory (Ok directory) ->
            ( { model | paintings = directory.paintings }, Cmd.none )

        GotDirectory (Err _) ->
            ( model, Cmd.none )
