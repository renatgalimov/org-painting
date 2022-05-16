module Painting exposing (main)

import Html exposing (Html, text)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, fail, field, list, maybe, oneOf, string, succeed)
import Maybe
import Maybe.Extra
import TimeTravel.Browser as TimeTravel exposing (defaultConfig)
import Url
import Url.Parser exposing ((</>), (<?>), parse, query, s, top)
import Url.Parser.Query as Query


main =
    TimeTravel.element Debug.toString
        Debug.toString
        defaultConfig
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


getJsonPath : String -> Maybe String
getJsonPath urlString =
    Url.fromString urlString
        |> Maybe.andThen (parse (s "painting.html" </> query (Query.string "json")))
        |> Maybe.Extra.join


init : String -> ( Model, Cmd Msg )
init location =
    ( { painting = Nothing }
    , getJsonPath location
        |> Maybe.map getPainting
        |> Maybe.withDefault Cmd.none
    )


type Msg
    = GotPainting (Result Http.Error Painting)


type TodoState
    = TodoTodo
    | TodoDone


type alias Todo =
    { title : String
    , state : TodoState
    , images : List Image
    }


type alias Image =
    { path : String
    }


type alias Painting =
    { title : String
    , todos : List Todo
    }


decodeImage : Decoder Image
decodeImage =
    Json.Decode.map Image (field "path" string)


todoStateToString : String -> Decoder TodoState
todoStateToString todoStateStr =
    case todoStateStr of
        "todo" ->
            succeed TodoTodo

        "done" ->
            succeed TodoDone

        _ ->
            fail ("Unknown todo state: \"" ++ todoStateStr ++ "\"")


decodeTodoState : Decoder TodoState
decodeTodoState =
    string
        |> Json.Decode.andThen todoStateToString


decodeTodo : Decoder Todo
decodeTodo =
    Json.Decode.map3 Todo
        (field "title" string)
        (field "state" decodeTodoState)
        (oneOf
            [ field "images" (list decodeImage)
            , succeed []
            ]
        )


decodePainting : Decoder Painting
decodePainting =
    Json.Decode.map2 Painting
        (field "title" string)
        (field "todo-list" (list decodeTodo))


getPainting : String -> Cmd Msg
getPainting jsonPath =
    Http.get
        { url = jsonPath
        , expect = expectJson GotPainting decodePainting
        }


type alias Model =
    { painting : Maybe Painting
    }



-- VIEW


viewPainting : Painting -> Html msg
viewPainting painting =
    text painting.title


view : Model -> Html msg
view model =
    case model.painting of
        Just painting ->
            viewPainting painting

        Nothing ->
            text "Waiting for painting to load..."



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPainting (Ok painting) ->
            ( { model | painting = Just painting }, Cmd.none )

        GotPainting (Err _) ->
            ( model, Cmd.none )
