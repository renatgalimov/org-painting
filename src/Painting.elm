module Painting exposing (main)

import Http
import Json.Decode exposing (Decoder, andThen, fail, field, list, string, succeed)
import Json.Decode.Extra exposing (fromResult)
import Maybe exposing (andThen)
import TimeTravel.Browser as TimeTravel exposing (defaultConfig)
import Url
import Url.Parser exposing (parse)
import Url.Parser.Query as Query



-- main =
--     TimeTravel.element Debug.toString
--         Debug.toString
--         { init = init
--         }


getJsonPath : String -> unknown
getJsonPath urlString =
    Url.fromString url
        |> Maybe.andThen Url.Parser.parse (Query.string "json")


init : String -> ( Model, Cmd Msg )
init location =
    ( { painting = Nothing }, Cmd.none )


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
        |> andThen todoStateToString


decodeTodo : Decoder Todo
decodeTodo =
    Json.Decode.map3 Todo
        (field "title" string)
        (field "state" decodeTodoState)
        (field "images" (list decodeImage))


decodePainting : Decoder Painting
decodePainting =
    Json.Decode.map2 Painting
        (field "title" string)
        (field "todo-list" (list decodeTodo))


getPainting =
    Http.get
        { url = paintingFile
        , expect = expectJson GotDirectory directoryDecoder
        }


type alias Model =
    { painting : Maybe Painting
    }
