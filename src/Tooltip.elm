module Tooltip exposing (Dimensions, Position(..), view)

import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)


type Position
    = AboveRight { y : Float, x : Float }


type alias Dimensions =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


view :
    { position : Maybe Position
    , trigger : Html msg
    , onPress : Dimensions -> msg
    , content : Html msg
    }
    -> Html msg
view options =
    Html.button
        [ Html.Events.on "click" (decodeClick options.onPress)
        , Html.Attributes.style "background" "none"
        , Html.Attributes.style "border" "none"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "margin" "0"
        ]
        [ options.trigger
        , case options.position of
            Nothing ->
                Html.text ""

            Just (AboveRight pos) ->
                Html.node "elm-portal"
                    []
                    [ Html.div
                        [ Html.Attributes.style "position" "fixed"
                        , Html.Attributes.style "top" (String.fromFloat pos.y ++ "px")
                        , Html.Attributes.style "left" (String.fromFloat pos.x ++ "px")
                        ]
                        [ options.content ]
                    ]
        ]


decodeClick : (Dimensions -> msg) -> Decoder msg
decodeClick toMsg =
    Json.Decode.map4
        (\x y width height -> toMsg { x = x, y = y, width = width, height = height })
        (Json.Decode.at [ "currentTarget", "___getBoundingClientRect", "left" ] Json.Decode.float)
        (Json.Decode.at [ "currentTarget", "___getBoundingClientRect", "top" ] Json.Decode.float)
        (Json.Decode.at [ "currentTarget", "___getBoundingClientRect", "width" ] Json.Decode.float)
        (Json.Decode.at [ "currentTarget", "___getBoundingClientRect", "height" ] Json.Decode.float)
