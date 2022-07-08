module Painting exposing (main)

import Css exposing (column, displayFlex, flexDirection, flexFlow1, fontSize, height, padding, pct, position, relative, rem, width)
import Funnels.PictureUrl
import Html.Styled exposing (Html, a, button, div, styled, text)
import Html.Styled.Attributes exposing (class, disabled, title)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, fail, field, list, maybe, string, succeed)
import Json.Encode exposing (Value)
import List.Extra
import Maybe
import Maybe.Extra
import Models.Image exposing (Image)
import PortFunnels exposing (FunnelDict, Handler(..), State)
import TimeTravel.Browser as TimeTravel exposing (defaultConfig)
import Url
import Url.Parser exposing ((</>), (<?>), parse, query, s)
import Url.Parser.Query as Query
import Views.ListWidget as ListWidget
import Views.OverlayWidget as OverlayWidget exposing (OverlayModel)
import Views.Sidebar as Sidebar
import Views.TitleBar as TitleBar exposing (breadcrumb)


main =
    TimeTravel.element Debug.toString
        Debug.toString
        defaultConfig
        { init = init
        , view = lazy view >> Html.Styled.toUnstyled
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


getTodoName : String -> Maybe String
getTodoName urlString =
    Url.fromString urlString
        |> Maybe.andThen (parse (s "painting.html" </> query (Query.string "todo")))
        |> Maybe.Extra.join
        |> Maybe.map (String.replace "+" " ")
        |> Maybe.andThen Url.percentDecode


init : String -> ( Model, Cmd Msg )
init location =
    let
        jsonPath =
            getJsonPath location

        todoName =
            Debug.log "ToDo name" (getTodoName location)
    in
    ( { state = PortFunnels.initialState
      , painting = Nothing
      , widget = ListWidget
      , currentTodoName = todoName
      , error = Nothing
      , sidebar = sidebarModel
      }
    , jsonPath
        |> Maybe.map getPainting
        |> Maybe.withDefault Cmd.none
    )


type Msg
    = Process Value
    | Overlay OverlayWidget.Msg
    | ToggleSidebar
    | GotPainting (Result Http.Error Painting)
    | SelectTodo (Maybe Todo)
    | SelectWidget Widget


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
        (maybe (field "caption" string))


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
        (Json.Decode.map emptyListOnMissing (maybe (field "image-list" (list decodeImage))))


emptyListOnMissing : Maybe (List item) -> List item
emptyListOnMissing maybeList =
    case maybeList of
        Just list ->
            list

        Nothing ->
            []


decodePainting : String -> Decoder Painting
decodePainting directory =
    Json.Decode.map4 Painting
        (field "title" string)
        (field "todo-list" (list decodeTodo))
        (Json.Decode.map emptyListOnMissing (maybe (field "image-list" (list decodeImage))))
        (succeed directory)


getPainting : String -> Cmd Msg
getPainting jsonPath =
    Http.get
        { url = jsonPath
        , expect = expectJson GotPainting (decodePainting <| dirname jsonPath)
        }


type Widget
    = ListWidget
    | OverlayWidget OverlayModel



-- MODEL


type alias Model =
    { state : State
    , widget : Widget
    , painting : Maybe Painting
    , currentTodoName : Maybe String
    , error : Maybe String
    , sidebar : SidebarModel
    }


getCurrentTodo : Model -> Maybe Todo
getCurrentTodo model =
    model.currentTodoName
        |> Maybe.andThen
            (\todoName ->
                model.painting
                    |> Maybe.map
                        (\painting -> painting.todos)
                    |> Maybe.andThen (List.Extra.find (\todo -> todo.title == todoName))
            )


{-| Get current image list for the model according to the model
configuration.
-}
getImages : Model -> Maybe (List Image)
getImages model =
    model.painting
        |> Maybe.andThen
            (\painting ->
                case getCurrentTodo model of
                    Just todo ->
                        Just todo.images

                    Nothing ->
                        Just painting.images
            )


withPainting : Maybe Painting -> Model -> Model
withPainting painting model =
    { model | painting = painting }


withCurrentTodo : Maybe Todo -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withCurrentTodo maybeTodo ( model, cmd ) =
    let
        ( todoString, currentTodoName ) =
            case maybeTodo of
                Just todo ->
                    ( todo.title, Just todo.title )

                Nothing ->
                    ( "", Nothing )

        widget =
            case model.widget of
                ListWidget ->
                    model.widget

                OverlayWidget overlayModel ->
                    getImages model
                        |> Maybe.andThen
                            (\images ->
                                OverlayWidget.withImages images overlayModel
                                    |> Maybe.map OverlayWidget
                            )
                        |> Maybe.withDefault ListWidget
    in
    ( { model | currentTodoName = currentTodoName, widget = widget }
    , Cmd.batch
        [ cmd
        , Funnels.PictureUrl.send getCmd <| Funnels.PictureUrl.makeQueryUpdateMessage "todo" todoString
        ]
    )



-- VIEW


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
    Sidebar.todo
        [ onClick (SelectTodo (Just todo))
        , title todo.title
        ]
        [ text (todoStateStr ++ ": " ++ icon ++ todo.title) ]


viewTodoList : List Todo -> Html Msg
viewTodoList todos =
    div [ class "todos" ] (List.map viewTodoBrief todos)


viewTitleBarPainting : Painting -> Maybe Todo -> Html Msg
viewTitleBarPainting painting currentTodo =
    breadcrumb [ class "breadcrumb" ]
        [ case currentTodo of
            Just _ ->
                a [ onClick (SelectTodo Nothing) ] [ text painting.title ]

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
            breadcrumb [ class "breadcrumb" ] [ text todo.title ]

        Nothing ->
            emptyHtml


viewTitleBar : Painting -> Maybe Todo -> Html Msg
viewTitleBar painting currentTodo =
    TitleBar.widget
        [ viewTitleBarPainting painting currentTodo
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


viewSidebar : Bool -> Maybe Todo -> Painting -> Html Msg
viewSidebar expanded currentTodo painting =
    let
        images =
            currentTodo
                |> Maybe.map (\todo -> todo.images)
                |> Maybe.withDefault painting.images
    in
    Sidebar.sidebar expanded
        []
        [ Sidebar.topLevelLink [ onClick (SelectTodo Nothing) ] [ text "Top level" ]
        , viewTodoList painting.todos
        , Sidebar.toggle [ class "sidebar-toggle", onClick ToggleSidebar ]
            [ text
                (if expanded then
                    "<"

                 else
                    ">"
                )
            ]
        , viewWidgetToggle images
        ]


type alias Directory =
    String


workzoneCss : Css.Style
workzoneCss =
    Css.batch
        [ displayFlex
        , flexDirection column
        , position relative
        , width (pct 100)
        ]


workzoneTitleCss : Css.Style
workzoneTitleCss =
    Css.batch
        [ padding (rem 1)
        , fontSize (rem 2.5)
        ]


viewWorkzone : Directory -> String -> List Image -> Widget -> Html Msg
viewWorkzone directory title images widget =
    let
        widgetHtml =
            case widget of
                ListWidget ->
                    ListWidget.view directory images

                OverlayWidget overlayModel ->
                    OverlayWidget.view directory overlayModel Overlay
    in
    styled div
        [ workzoneCss ]
        [ class "workzone" ]
        [ styled div [ workzoneTitleCss ] [] [ text title ]
        , widgetHtml
        ]



-- Views


view : Model -> Html Msg
view model =
    let
        currentTodo =
            getCurrentTodo model
    in
    case model.painting of
        Just painting ->
            let
                ( images, title ) =
                    case getCurrentTodo model of
                        Just todo ->
                            ( Debug.log "images" todo.images, todo.title )

                        Nothing ->
                            ( painting.images, painting.title )
            in
            styled div
                [ height (pct 100), flexFlow1 column, displayFlex ]
                [ class "screen" ]
                [ viewTitleBar painting currentTodo
                , styled div
                    [ displayFlex, height (pct 100) ]
                    [ class "sidebar-and-workzone" ]
                    [ viewSidebar model.sidebar.expanded currentTodo painting
                    , viewWorkzone painting.directory title images model.widget
                    ]
                ]

        Nothing ->
            case model.error of
                Just error ->
                    text error

                Nothing ->
                    text "Waiting for painting to load..."


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
        Overlay (OverlayWidget.Toggled toggled) ->
            case model.widget of
                OverlayWidget overlayModel ->
                    ( { model | widget = OverlayWidget { overlayModel | toggled = toggled } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleSidebar ->
            ( { model | sidebar = sidebarToggleExpanded model.sidebar }, Cmd.none )

        GotPainting (Ok painting) ->
            ( model |> withPainting (Just painting), Cmd.none )

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
            withCurrentTodo todo ( model, Cmd.none )

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

        SelectWidget widget ->
            ( { model | widget = widget }, Cmd.none )



-- Widget toggle


{-| A group of buttons change to select a currently displayed widget
-}
viewWidgetToggle : List Image -> Html Msg
viewWidgetToggle images =
    let
        widgetToggleCss =
            Css.batch
                [ displayFlex
                , padding (rem 1)
                ]
    in
    styled div
        [ widgetToggleCss ]
        [ class "widget-toggle" ]
        [ button [ class "list-widget", onClick (SelectWidget ListWidget) ] [ text "List" ]
        , button
            [ class "overlay-widget"
            , case OverlayWidget.newModel images of
                Just overlayModel ->
                    onClick <| SelectWidget (OverlayWidget overlayModel)

                _ ->
                    disabled True
            ]
            [ text "Overlay" ]
        ]
