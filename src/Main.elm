module Main exposing (Model, Msg, main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Length exposing (Length)
import LineSegment2d
import Point2d
import Svg
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- TYPES


type alias Model =
    { quiltWidth : ( String, Length )
    , quiltHeight : ( String, Length )
    , zoom : Float
    }



-- INIT


init : Model
init =
    { quiltWidth = ( "1", Length.meters 1 )
    , quiltHeight = ( "2", Length.meters 2 )
    , zoom = 1
    }



-- UPDATE


type Msg
    = GotWidth String
    | GotHeight String
    | GotZoom Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotWidth widthStr ->
            { model
                | quiltWidth =
                    ( widthStr
                    , case String.toFloat widthStr of
                        Nothing ->
                            Tuple.second model.quiltWidth

                        Just width ->
                            Length.meters width
                    )
            }

        GotHeight heightStr ->
            { model
                | quiltHeight =
                    ( heightStr
                    , case String.toFloat heightStr of
                        Nothing ->
                            Tuple.second model.quiltHeight

                        Just height ->
                            Length.meters height
                    )
            }

        GotZoom zoom ->
            { model | zoom = zoom }



-- VIEW


view : Model -> Html Msg
view model =
    layout []
        (viewModel model)


viewModel : Model -> Element Msg
viewModel model =
    column
        [ width fill ]
        [ row
            [ paddingXY 16 8
            , Background.color (rgb 0.8 0.9 0.7)
            , width fill
            ]
            [ text "Quilt Designer" ]
        , row
            []
            [ column
                [ padding 16
                , spacing 8
                , width fill
                , alignTop
                ]
                [ Input.text
                    []
                    { onChange = GotWidth
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Width (m)")
                    , text = Tuple.first model.quiltWidth
                    }
                , Input.text
                    []
                    { onChange = GotHeight
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Height (m)")
                    , text = Tuple.first model.quiltHeight
                    }
                , Input.slider
                    [ behindContent
                        (el
                            [ width fill
                            , height (px 2)
                            , centerY
                            , Background.color (rgb 0.2 0.2 0.2)
                            , Border.rounded 2
                            ]
                            none
                        )
                    ]
                    { label = Input.labelAbove [] (text "Zoom")
                    , max = 10
                    , min = 1
                    , onChange = GotZoom
                    , step = Just 0.25
                    , thumb = Input.defaultThumb
                    , value = model.zoom
                    }
                ]
            , viewDesigner model
                |> html
                |> el [ alignTop ]
            ]
        ]


viewDesigner : Model -> Html Msg
viewDesigner model =
    let
        widthStr : String
        widthStr =
            model.quiltWidth
                |> Tuple.second
                |> Length.inMeters
                |> (*) 100
                |> (*) model.zoom
                |> String.fromFloat

        heightStr : String
        heightStr =
            model.quiltHeight
                |> Tuple.second
                |> Length.inMeters
                |> (*) 100
                |> (*) model.zoom
                |> String.fromFloat
    in
    Svg.svg
        [ Svg.Attributes.width widthStr
        , Svg.Attributes.height heightStr
        , Svg.Attributes.viewBox (String.join " " [ "0", "0", widthStr, heightStr ])
        , Html.Attributes.style "border" "1px solid black"
        ]
        [ Geometry.Svg.lineSegment2d
            [ Svg.Attributes.stroke "blue"
            , Svg.Attributes.strokeWidth "1"
            ]
            (LineSegment2d.from
                (Point2d.meters 0 0)
                (Point2d.meters (50 * model.zoom) (100 * model.zoom))
            )
        ]
