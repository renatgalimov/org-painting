module Funnels.PictureUrl exposing
    ( Message(..), Response, State
    , moduleName, moduleDesc, commander
    , initialState
    , send
    , toString, toJsonString
    , makeQueryUpdateMessage
    )

{-| An example add/multiply funnel, with a simulator.


# Types

@docs Message, Response, State


# Components of a `PortFunnel.FunnelSpec`

@docs moduleName, moduleDesc, commander


# Initial `State`

@docs initialState


# Sending a `Message` out the `Cmd` Port

@docs makeAddMessage, makeMultiplyMessage, send


# Conversion to Strings

@docs toString, toJsonString


# Simulator

@docs makeSimulatedCmdPort


# Non-standard Functions

@docs stateToStrings

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import PortFunnel exposing (GenericMessage, ModuleDesc)


type alias QueryUpdate =
    ( String, String )


{-| Our internal state.

Just tracks all incoming messages.

-}
type alias State =
    Dict String String


{-| A `MessageResponse` encapsulates a message.

`NoResponse` is currently unused, but many PortFunnel-aware modules will need it.

-}
type alias Response =
    ()


{-| `AddMessage` and `MultiplyMessage` go out from Elm to the JS.

`SumMessage` and `ProductMessage` come back in.

-}
type Message
    = QueryUpdateMessage QueryUpdate


{-| The initial, empty state, so the application can initialize its state.
-}
initialState : State
initialState =
    Dict.empty


{-| The name of this module: "AddXY".
-}
moduleName : String
moduleName =
    "PictureUrl"


{-| Our module descriptor.
-}
moduleDesc : ModuleDesc Message State Response
moduleDesc =
    PortFunnel.makeModuleDesc moduleName encode decode process


encode : Message -> GenericMessage
encode message =
    case message of
        QueryUpdateMessage ( key, value ) ->
            GenericMessage moduleName
                "set"
            <|
                JE.object [ ( key, JE.string value ) ]


listToQueryUpdate : List ( String, String ) -> Decoder QueryUpdate
listToQueryUpdate items =
    List.head items
        |> Maybe.map JD.succeed
        |> Maybe.withDefault (JD.fail "Unexpected QueryUpdate")


queryUpdateDecoder : Decoder Message
queryUpdateDecoder =
    JD.keyValuePairs JD.string
        |> JD.andThen listToQueryUpdate
        |> JD.map (\queryUpdate -> QueryUpdateMessage queryUpdate)


decode : GenericMessage -> Result String Message
decode { tag, args } =
    case tag of
        "set" ->
            decodeValue queryUpdateDecoder args

        _ ->
            Err <| "Unknown QueryUpdate tag: " ++ tag


decodeValue : Decoder x -> Value -> Result String x
decodeValue decoder value =
    case JD.decodeValue decoder value of
        Ok x ->
            Ok x

        Err err ->
            Err <| JD.errorToString err


{-| Send a `Message` through a `Cmd` port.
-}
send : (Value -> Cmd msg) -> Message -> Cmd msg
send =
    PortFunnel.sendMessage moduleDesc


process : Message -> State -> ( State, Response )
process message state =
    case message of
        QueryUpdateMessage queryUpdate ->
            Debug.log "PictureUrl process" <| ( Dict.insert (Tuple.first queryUpdate) (Tuple.second queryUpdate) state, () )


{-| Responsible for sending a `CmdResponse` back througt the port.

Called by `PortFunnel.appProcess` for each response returned by `process`.

The `AddXY` module doesn't send itself messages, so this is just `PortFunnel.emptyCommander`.

-}
commander : (GenericMessage -> Cmd msg) -> Response -> Cmd msg
commander =
    PortFunnel.emptyCommander


{-| Convert a `Message` to a nice-looking human-readable string.
-}
toString : Message -> String
toString message =
    case message of
        QueryUpdateMessage ( key, value ) ->
            "Query update: " ++ key ++ " -> " ++ value


{-| Convert a `Message` to the same JSON string that gets sent

over the wire to the JS code.

-}
toJsonString : Message -> String
toJsonString message =
    encode message
        |> PortFunnel.encodeGenericMessage
        |> JE.encode 0


makeQueryUpdateMessage : String -> String -> Message
makeQueryUpdateMessage key value =
    QueryUpdateMessage ( key, value )
