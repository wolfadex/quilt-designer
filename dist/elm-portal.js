Object.defineProperty(Element.prototype, '___getBoundingClientRect', {
  get() {
    return this.getBoundingClientRect()
  },
})

const portalZoneId = 'elm-portal-target'

window.customElements.define(
  'elm-portal',
  class extends HTMLElement {
    // Base custom element stuff
    connectedCallback() {
      this._targetNode = document.createElement('div')
      document.getElementById(portalZoneId).appendChild(this._targetNode)
    }

    disconnectedCallback() {
      document.getElementById(portalZoneId).removeChild(this._targetNode)
    }

    // Re-implementations of HTMLElement functions
    get childNodes() {
      return this._targetNode.childNodes
    }

    replaceData(...args) {
      return this._targetNode.replaceData(...args)
    }

    removeChild(...args) {
      return this._targetNode.removeChild(...args)
    }

    insertBefore(...args) {
      return this._targetNode.insertBefore(...args)
    }
    appendChild(...args) {
      // To cooperate with the Elm runtime
      requestAnimationFrame(() => {
        return this._targetNode.appendChild(...args)
      })
    }
  },
)

window.customElements.define(portalZoneId, class extends HTMLElement {})

/*
module Dropdown exposing (Model, Msg, init, update, view, viewAllContent)

import AssocList exposing (Dict)
import Html exposing (..)
import Html.Attributes
import Html.Events
import Json.Decode as JD exposing (Decoder)


type Msg k
    = Open k Pos
    | Close k


type alias Pos =
    { x : Float
    , y : Float
    }


type Model k
    = Model (Internal k)


type alias Internal k =
    Dict k Pos


init : Model k
init =
    Model AssocList.empty


update : Msg k -> Model k -> Model k
update msg (Model model) =
    (case msg of
        Open did pos ->
            AssocList.insert did pos model

        Close did ->
            AssocList.remove did model
    )
        |> Model


view :
    { content : Html msg
    , triggerId : k
    , trigger : Html msg
    , toggle : Msg k -> msg
    }
    -> Model k
    -> Html msg
view options (Model model) =
    let
        isOpen : Bool
        isOpen =
            AssocList.member options.triggerId model
    in
    button
        [ Html.Events.on "click" (decodeClick options.triggerId isOpen options.toggle)
        ]
        [ options.trigger
        , case AssocList.get options.triggerId model of
            Just { x, y } ->
                node "elm-portal"
                    []
                    [ div
                        [ Html.Attributes.style "position" "fixed"
                        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
                        , Html.Attributes.style "top" (String.fromFloat y ++ "px")
                        ]
                        [ options.content ]
                    ]

            _ ->
                text ""
        ]


decodeClick : k -> Bool -> (Msg k -> msg) -> Decoder msg
decodeClick did isOpen toMsg =
    if isOpen then
        JD.succeed (toMsg (Close did))

    else
        JD.map2
            (\x y -> toMsg (Open did { x = x, y = y }))
            (JD.at [ "currentTarget", "___getBoundingClientRect", "left" ] JD.float)
            (JD.at [ "currentTarget", "___getBoundingClientRect", "bottom" ] JD.float)


viewAllContent : List String -> Html msg
viewAllContent =
    List.map
        (\did ->
            div [ Html.Attributes.id ("elm-portal_dropdown-id_" ++ did) ] []
        )
        >> div []

*/
