module Painting exposing (main, viewPainting)

import Html exposing (Html, div, h1, h2, img, span, text)
import Html.Attributes exposing (class, src, width)
import Html.Events exposing (onClick)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, fail, field, list, oneOf, string, succeed)
import List.Extra exposing (find)
import Maybe
import Maybe.Extra
import TimeTravel.Browser as TimeTravel exposing (defaultConfig)
import Url
import Url.Builder
import Url.Parser exposing ((</>), (<?>), parse, query, s)
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


dirname : String -> String
dirname input =
    String.split "/" input
        |> List.reverse
        |> List.tail
        |> Maybe.withDefault []
        |> List.reverse
        |> String.join "/"


getJsonPath : String -> Maybe String
getJsonPath urlString =
    Url.fromString urlString
        |> Maybe.andThen (parse (s "painting.html" </> query (Query.string "json")))
        |> Maybe.Extra.join


init : String -> ( Model, Cmd Msg )
init location =
    let
        jsonPath =
            getJsonPath location
    in
    ( { painting = Nothing
      , currentTodo = Nothing
      , widget = Nothing
      }
    , jsonPath
        |> Maybe.map getPainting
        |> Maybe.withDefault Cmd.none
    )


type Msg
    = GotPainting (Result Http.Error Painting)
    | SelectTodo Todo


type alias Image =
    { path : String
    }


type alias Todo =
    { title : String
    , state : TodoState
    , images : List Image
    }


type TodoState
    = TodoTodo
    | TodoDone


todoStateToString : TodoState -> String
todoStateToString todoState =
    case todoState of
        TodoTodo ->
            "todo"

        TodoDone ->
            "done"


type alias Painting =
    { title : String
    , todos : List Todo
    , directory : String
    }


decodeImage : Decoder Image
decodeImage =
    Json.Decode.map Image (field "path" string)


todoStateFromString : String -> Decoder TodoState
todoStateFromString todoStateStr =
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
        |> Json.Decode.andThen todoStateFromString


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


decodePainting : String -> Decoder Painting
decodePainting directory =
    Json.Decode.map3 Painting
        (field "title" string)
        (field "todo-list" (list decodeTodo))
        (succeed directory)


getPainting : String -> Cmd Msg
getPainting jsonPath =
    Http.get
        { url = jsonPath
        , expect = expectJson GotPainting (decodePainting <| dirname jsonPath)
        }


type alias ImageWidget =
    { currentImage : Image
    , allImages : List Image
    }


type Widget
    = WidgetImage ImageWidget


type alias Model =
    { painting : Maybe Painting
    , currentTodo : Maybe Todo
    , widget : Maybe Widget
    }



-- VIEW


viewImage : String -> Image -> Html msg
viewImage directory image =
    img [ src (Url.Builder.relative [ directory, image.path ] []) ] []


viewTodoBrief : Todo -> Html Msg
viewTodoBrief todo =
    let
        todoStateStr =
            todoStateToString todo.state

        icon =
            case List.length todo.images of
                0 ->
                    " "

                _ ->
                    "🖼 "
    in
    div
        [ onClick (SelectTodo todo)
        , class ("todo-brief " ++ todoStateStr)
        ]
        [ text (todoStateStr ++ ": " ++ icon ++ todo.title) ]


viewTodo : String -> Todo -> Html msg
viewTodo directory todo =
    let
        todoStateStr =
            todoStateToString todo.state
    in
    div [ class ("todo " ++ todoStateStr) ]
        (h2 [ class "title" ] [ text (todoStateStr ++ ": " ++ todo.title) ]
            :: List.map (viewImage directory) todo.images
        )


viewTodoList : List Todo -> Html Msg
viewTodoList todos =
    div [ class "todos" ] (List.map viewTodoBrief todos)


viewPainting : Painting -> Html Msg
viewPainting painting =
    div [ class "painting" ]
        [ viewTodoList painting.todos
        ]


viewTitleBar : Painting -> Maybe Todo -> Html Msg
viewTitleBar painting currentTodo =
    div [ class "title-bar row" ]
        [ text (currentTodo |> Maybe.map (\todo -> todo.title) |> Maybe.withDefault painting.title) ]



-- Views


view : Model -> Html Msg
view model =
    case model.painting of
        Just painting ->
            div [ class "container screen" ]
                [ viewTitleBar painting model.currentTodo
                , case model.currentTodo of
                    Just todo ->
                        case List.head todo.images of
                            Just image ->
                                viewImageWidget painting.directory image todo.images

                            Nothing ->
                                viewTodoList painting.todos

                    Nothing ->
                        viewTodoList painting.todos
                ]

        Nothing ->
            text "Waiting for painting to load..."


{-| Display a painting widget which takes all the free space on the screen
-}
viewImageWidget : String -> Image -> List Image -> Html Msg
viewImageWidget directory image images =
    div [ class "painting-widget" ]
        [ div [ class "current-image" ]
            [ viewImage
                directory
                image
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPainting (Ok painting) ->
            ( { model | painting = Just painting }, Cmd.none )

        GotPainting (Err _) ->
            ( model, Cmd.none )

        SelectTodo todo ->
            ( { model | currentTodo = Just todo }, Cmd.none )
