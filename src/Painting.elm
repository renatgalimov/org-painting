module Painting exposing (main)

import Funnels.PictureUrl
import Html exposing (Html, a, button, div, img, span, text)
import Html.Attributes exposing (class, classList, href, src, title)
import Html.Events exposing (onClick)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, fail, field, list, nullable, oneOf, string, succeed)
import Json.Encode exposing (Value)
import Maybe
import Maybe.Extra
import PortFunnels exposing (FunnelDict, Handler(..), State)
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
        , subscriptions = PortFunnels.subscriptions Process
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
    ( { state = PortFunnels.initialState
      , painting = Nothing
      , currentTodo = Nothing
      , error = Nothing
      , sidebar = sidebarModel
      }
    , jsonPath
        |> Maybe.map getPainting
        |> Maybe.withDefault Cmd.none
    )


type Msg
    = Process Value
    | ToggleSidebar
    | GotPainting (Result Http.Error Painting)
    | SelectTodo Todo
    | ClearTodo


type alias Image =
    { path : String
    , caption : Maybe String
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
    , images : List Image
    , directory : String
    }


decodeImage : Decoder Image
decodeImage =
    Json.Decode.map2 Image
        (field "path" string)
        (field "caption" (nullable string))


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
    Json.Decode.map4 Painting
        (field "title" string)
        (field "todo-list" (list decodeTodo))
        (field "image-list" (list decodeImage))
        (succeed directory)


getPainting : String -> Cmd Msg
getPainting jsonPath =
    Http.get
        { url = jsonPath
        , expect = expectJson GotPainting (decodePainting <| dirname jsonPath)
        }


type alias Model =
    { state : State
    , painting : Maybe Painting
    , currentTodo : Maybe Todo
    , error : Maybe String
    , sidebar : SidebarModel
    }


setSidebar : SidebarModel -> Model -> Model
setSidebar newSidebar model =
    { model
        | sidebar = newSidebar
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
        , title todo.title
        ]
        [ text (todoStateStr ++ ": " ++ icon ++ todo.title) ]


viewTodoList : List Todo -> Html Msg
viewTodoList todos =
    div [ class "todos" ] (List.map viewTodoBrief todos)


viewTitleBarLogo : Html Msg
viewTitleBarLogo =
    span [ class "logo" ]
        [ text "Ink and Zett"
        ]


viewTitleBarIndex : Html Msg
viewTitleBarIndex =
    span [ class "breadcrumb" ]
        [ a [ href "/index.html" ]
            [ text "Paintings"
            ]
        ]


viewTitleBarPainting : Painting -> Maybe Todo -> Html Msg
viewTitleBarPainting painting currentTodo =
    span [ class "breadcrumb" ]
        [ case currentTodo of
            Just _ ->
                a [ onClick ClearTodo ] [ text painting.title ]

            Nothing ->
                text painting.title
        ]


emptyHtml : Html Msg
emptyHtml =
    text ""


viewTitleBarTodo : Maybe Todo -> Html Msg
viewTitleBarTodo currentTodo =
    case currentTodo of
        Just todo ->
            span [ class "breadcrumb" ] [ text todo.title ]

        Nothing ->
            emptyHtml


viewTitleBar : Painting -> Maybe Todo -> Html Msg
viewTitleBar painting currentTodo =
    div [ class "title-bar" ]
        [ viewTitleBarLogo
        , viewTitleBarIndex
        , viewTitleBarPainting painting currentTodo
        , viewTitleBarTodo currentTodo
        ]


type alias SidebarModel =
    { expanded : Bool
    }


sidebarModel : SidebarModel
sidebarModel =
    { expanded = False }


sidebarToggleExpanded : SidebarModel -> SidebarModel
sidebarToggleExpanded sidebar =
    { sidebar | expanded = not sidebar.expanded }


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ classList [ ( "sidebar", True ), ( "expanded", model.sidebar.expanded ) ] ]
        [ a [ class "top-level-link" ] [ text "Top level" ]
        , case model.painting of
            Just painting ->
                viewTodoList painting.todos

            Nothing ->
                emptyHtml
        , button [ class "sidebar-toggle", onClick ToggleSidebar ]
            [ text
                (if model.sidebar.expanded then
                    "<"

                 else
                    ">"
                )
            ]
        ]



-- Views


view : Model -> Html Msg
view model =
    case model.painting of
        Just painting ->
            div [ class "container screen" ]
                [ viewTitleBar painting model.currentTodo
                , div [ class "sidebar-and-workzone" ]
                    [ viewSidebar model
                    ]
                ]

        Nothing ->
            case model.error of
                Just error ->
                    text error

                Nothing ->
                    text "Waiting for painting to load..."


viewImageWidgetItem : String -> Image -> Html Msg
viewImageWidgetItem directory image =
    div [ class "painting-widget-item" ]
        [ viewImage directory image
        ]


{-| Display a painting widget which takes all the free space on the screen
-}
viewImageWidget : String -> Image -> List Image -> Html Msg
viewImageWidget directory _ images =
    div [ class "painting-widget" ]
        (div [ class "pusher" ] []
            :: List.map
                (viewImageWidgetItem directory)
                images
            ++ [ div [ class "pusher" ] [] ]
        )


pictureUrlHandler : Funnels.PictureUrl.Response -> State -> Model -> ( Model, Cmd Msg )
pictureUrlHandler _ state model =
    ( { model | state = state }, Cmd.none )


handlers : List (Handler Model Msg)
handlers =
    [ PictureUrlHandler pictureUrlHandler
    ]


{-| Get a possibly simulated output port.
-}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


getCmd : Value -> Cmd Msg
getCmd =
    PortFunnels.getCmdPort Process "PictureUrl" False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers getCmdPort



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSidebar ->
            ( { model | sidebar = sidebarToggleExpanded model.sidebar }, Cmd.none )

        GotPainting (Ok painting) ->
            ( { model | painting = Just painting }, Cmd.none )

        GotPainting (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.BadBody badBodyError ->
                            badBodyError

                        _ ->
                            Debug.toString error
            in
            ( { model | error = Just errorMsg }, Cmd.none )

        SelectTodo todo ->
            ( { model | currentTodo = Just todo }, Funnels.PictureUrl.send getCmd <| Funnels.PictureUrl.makeQueryUpdateMessage "todo" todo.title )

        ClearTodo ->
            ( { model | currentTodo = Nothing }, Funnels.PictureUrl.send getCmd <| Funnels.PictureUrl.makeQueryUpdateMessage "todo" "" )

        Process value ->
            case
                PortFunnels.processValue funnelDict value model.state model
            of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok res ->
                    res
