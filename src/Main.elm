module Main exposing (Block, Model, Msg, main)

import Array exposing (Array)
import Array.Extra
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Json.Decode exposing (Decoder)
import Length exposing (Length, Meters)
import LineSegment2d
import Point2d
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Tooltip exposing (Dimensions, Position(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- TYPES


type alias Model =
    { quiltWidth : ( String, Length )
    , quiltHeight : ( String, Length )
    , zoom : Float
    , blocks : Array Block
    , activeBlock : Maybe Int
    , placing : Maybe Shape
    }


type alias Block =
    { width : ( String, Length )
    , height : ( String, Length )
    , label : String
    , confirmDelete : Maybe Dimensions
    , shapes : Array Shape
    }


type Shape
    = Rect (Rectangle2d Meters BlockCoordinates)


type BlockCoordinates
    = BlockCoordinates Never


type QuiltCoordinates
    = QuiltCoordinates Never



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { quiltWidth = ( "1", Length.meters 1 )
      , quiltHeight = ( "2", Length.meters 2 )
      , zoom = 1
      , blocks = Array.empty
      , activeBlock = Nothing
      , placing = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- case model.placing of
    --     Nothing -> Sub.none
    --     Just _ ->
    --         Browser.Events.onMouseMove decodeMouseMove
    Sub.none


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Json.Decode.map2 MouseMoved
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)



-- UPDATE


type Msg
    = GotWidth String
    | GotHeight String
    | GotZoom Float
    | CreateBlock
    | GotBlockName Int String
    | GotBlockWidth Int String
    | PromptDeleteBlock Int Dimensions
    | CancelDeleteBlock Int
    | ConfirmDeleteBlock Int
    | GotActiveBlock (Maybe Int)
    | SvgMouseDown Float Float
    | StartPlacing Shape
    | MouseMoved Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotActiveBlock activeBlock ->
            ( { model | activeBlock = activeBlock }, Cmd.none )

        GotWidth widthStr ->
            ( { model
                | quiltWidth =
                    ( widthStr
                    , case String.toFloat widthStr of
                        Nothing ->
                            Tuple.second model.quiltWidth

                        Just width ->
                            Length.meters width
                    )
              }
            , Cmd.none
            )

        GotHeight heightStr ->
            ( { model
                | quiltHeight =
                    ( heightStr
                    , case String.toFloat heightStr of
                        Nothing ->
                            Tuple.second model.quiltHeight

                        Just height ->
                            Length.meters height
                    )
              }
            , Cmd.none
            )

        GotZoom zoom ->
            ( { model | zoom = zoom }, Cmd.none )

        CreateBlock ->
            ( { model
                | blocks =
                    Array.push
                        { width = ( "0.25", Length.meters 0.25 )
                        , height = ( "0.25", Length.meters 0.25 )
                        , label = ""
                        , confirmDelete = Nothing
                        , shapes = Array.empty
                        }
                        model.blocks
                , activeBlock = Just (Array.length model.blocks)
              }
            , Cmd.none
            )

        GotBlockName index label ->
            ( { model
                | blocks =
                    Array.Extra.update index
                        (\block -> { block | label = label })
                        model.blocks
              }
            , Cmd.none
            )

        GotBlockWidth index widthStr ->
            ( { model
                | blocks =
                    Array.Extra.update index
                        (\block ->
                            { block
                                | width =
                                    ( widthStr
                                    , case String.toFloat widthStr of
                                        Nothing ->
                                            Tuple.second block.width

                                        Just width ->
                                            Length.meters width
                                    )
                            }
                        )
                        model.blocks
              }
            , Cmd.none
            )

        PromptDeleteBlock index dimensions ->
            ( { model
                | blocks =
                    Array.Extra.update index
                        (\block -> { block | confirmDelete = Just dimensions })
                        model.blocks
              }
            , Cmd.none
            )

        CancelDeleteBlock index ->
            ( { model
                | blocks =
                    Array.Extra.update index
                        (\block -> { block | confirmDelete = Nothing })
                        model.blocks
              }
            , Cmd.none
            )

        ConfirmDeleteBlock index ->
            ( { model
                | blocks =
                    Array.append
                        (Array.slice 0 index model.blocks)
                        (Array.slice (index + 1) (Array.length model.blocks) model.blocks)
              }
            , Cmd.none
            )

        SvgMouseDown x y ->
            let
                _ =
                    Debug.log "svg click" ( x, y )
            in
            case ( model.placing, model.activeBlock ) of
                ( Just shape, Just index ) ->
                    ( { model
                        | blocks =
                            Array.Extra.update index
                                (\block ->
                                    { block
                                        | shapes =
                                            Array.push
                                                (setShapeXY x y shape)
                                                block.shapes
                                    }
                                )
                                model.blocks
                        , placing = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StartPlacing shape ->
            ( { model | placing = Just shape }, Cmd.none )

        MouseMoved x y ->
            ( model, Cmd.none )


setShapeXY : Float -> Float -> Shape -> Shape
setShapeXY x y shape =
    case shape of
        Rect rect ->
            let
                ( w, h ) =
                    Rectangle2d.dimensions rect
            in
            Rect
                (Rectangle2d.with
                    { x1 = Length.meters x
                    , y1 = Length.meters y
                    , x2 =
                        Length.meters x
                            |> Quantity.plus w
                    , y2 =
                        Length.meters y
                            |> Quantity.plus h
                    }
                )



-- VIEW


view : Model -> Html Msg
view model =
    layout [] (viewModel model)


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
            [ width fill ]
            [ drawingPanel model
            , designerPanel model
            , propertiesPanel model
            ]
        ]


drawingPanel : Model -> Element Msg
drawingPanel model =
    column
        [ alignTop
        , padding 8
        , spacing 8
        ]
        [ text "Shapes"
        , Input.button
            []
            { label = text "Rectangle"
            , onPress =
                Just
                    (StartPlacing
                        (Rect
                            (Rectangle2d.with
                                { x1 = Length.meters 0
                                , y1 = Length.meters 0
                                , x2 = Length.meters 0.1
                                , y2 = Length.meters 0.1
                                }
                            )
                        )
                    )
            }
        ]


designerPanel : Model -> Element Msg
designerPanel model =
    column
        [ alignTop
        , width fill
        ]
        [ model.blocks
            |> Array.toIndexedList
            |> List.map (viewBlockTab model.activeBlock)
            |> (\a -> a ++ [ viewNewBlockTab ])
            |> (::) (viewQuiltTab model.activeBlock)
            |> List.intersperse tabSpacer
            |> row
                [ width fill
                ]
        , viewDesigner model
            (case model.activeBlock of
                Nothing ->
                    [ Geometry.Svg.lineSegment2d
                        [ Svg.Attributes.stroke "blue"
                        , Svg.Attributes.strokeWidth "1"
                        ]
                        (LineSegment2d.from
                            (Point2d.meters 0 0)
                            (Point2d.meters (50 * model.zoom) (100 * model.zoom))
                        )
                    ]

                Just index ->
                    case Array.get index model.blocks of
                        Nothing ->
                            []

                        Just block ->
                            block.shapes
                                |> Array.toIndexedList
                                |> List.map (viewShape model.zoom (Just index))
            )
            |> html
            |> el [ width fill, height fill ]
        ]


viewShape : Float -> Maybe Int -> ( Int, Shape ) -> Svg Msg
viewShape zoom blockIndex ( shapeIndex, shape ) =
    case shape of
        Rect rect ->
            let
                ( w, h ) =
                    Rectangle2d.dimensions rect

                scaleAmount =
                    canvasScale * zoom
                    -- zoom

                ( x1, y1 ) =
                    Rectangle2d.centerPoint rect
                        |> Point2d.coordinates
                        |> Tuple.mapBoth
                            (Quantity.minus (Quantity.divideBy 2 w))
                            (Quantity.minus (Quantity.divideBy 2 h))
            in
            Geometry.Svg.rectangle2d
                [ Svg.Attributes.stroke "blue"
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.fill "orange"
                ]
                -- (Rectangle2d.scaleAbout
                --     Point2d.origin
                --     (canvasScale * zoom)
                --     rect
                -- )
                (Rectangle2d.with
                    { x1 = x1
                    , y1 = y1
                    , x2 = Quantity.multiplyBy scaleAmount (Quantity.plus x1 w)
                    , y2 = Quantity.multiplyBy scaleAmount (Quantity.plus y1 h)
                    }
                )


viewNewBlockTab : Element Msg
viewNewBlockTab =
    Input.button
        [ paddingXY 16 8
        , Background.color (rgb 0.7 0.7 0.7)
        , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
        ]
        { onPress = Just CreateBlock
        , label = text "+"
        }


propertiesPanel : Model -> Element Msg
propertiesPanel model =
    column
        [ padding 16
        , spacing 8
        , width <| minimum 300 <| maximum 400 fill
        , alignTop
        ]
        ((case model.activeBlock of
            Nothing ->
                [ text "Quilt"
                , quiltProperties model
                ]

            Just index ->
                case Array.get index model.blocks of
                    Nothing ->
                        Debug.todo "missing block"

                    Just block ->
                        [ text "Block"
                        , viewBlockProperties ( index, block )
                        ]
         )
            ++ [ Input.slider
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
        )


tabSpacer : Element Msg
tabSpacer =
    el [ height fill, Border.width 1 ] none


viewQuiltTab : Maybe Int -> Element Msg
viewQuiltTab active =
    Input.button
        [ paddingXY 16 8
        , Background.color <|
            case active of
                Nothing ->
                    rgb 0.7 0.9 0.7

                Just _ ->
                    rgb 0.9 0.9 0.9
        , Border.roundEach
            { topLeft = 4
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 4
            }
        ]
        { onPress = Just (GotActiveBlock Nothing)
        , label = text "Quilt"
        }


viewBlockTab : Maybe Int -> ( Int, Block ) -> Element Msg
viewBlockTab active ( index, block ) =
    Input.button
        [ paddingXY 16 8
        , Background.color <|
            case active of
                Nothing ->
                    rgb 0.9 0.9 0.9

                Just id ->
                    if id == index then
                        rgb 0.7 0.9 0.7

                    else
                        rgb 0.9 0.9 0.9
        ]
        { onPress = Just (GotActiveBlock (Just index))
        , label = text (block.label ++ " (Block)")
        }


viewBlockProperties : ( Int, Block ) -> Element Msg
viewBlockProperties ( index, block ) =
    column
        [ spacing 8 ]
        [ row
            [ spacing 8 ]
            [ Input.text
                []
                { label = Input.labelHidden "block label"
                , placeholder = Just (Input.placeholder [] (text "Block Label"))
                , text = block.label
                , onChange = GotBlockName index
                }
            , Tooltip.view
                { onPress = PromptDeleteBlock index
                , position =
                    case block.confirmDelete of
                        Nothing ->
                            Nothing

                        Just pos ->
                            Just (AboveRight { x = pos.x, y = pos.y - pos.height })
                , trigger =
                    layoutWith { options = [ noStaticStyleSheet ] }
                        []
                        (el
                            [ paddingXY 16 8
                            , Border.width 3
                            , Border.color (rgb 0.5 0.7 0.6)
                            ]
                            (text "Delete")
                        )
                , content =
                    layoutWith { options = [ noStaticStyleSheet ] }
                        []
                        (row
                            [ padding 8
                            , spacing 8
                            , Background.color (rgb 0.1 0.1 0.1)
                            , Border.rounded 8
                            , Font.color (rgb 1 1 1)
                            ]
                            [ Input.button
                                []
                                { onPress = Just (CancelDeleteBlock index)
                                , label = text "Cancel"
                                }
                            , Input.button
                                []
                                { onPress = Just (ConfirmDeleteBlock index)
                                , label = text "Confirm"
                                }
                            ]
                        )
                }
                |> html
                |> el []
            ]
        , Input.text
            []
            { label = Input.labelAbove [] (text "Width (m)")
            , placeholder = Nothing
            , text = Tuple.first block.width
            , onChange = GotBlockWidth index
            }
        ]


quiltProperties : Model -> Element Msg
quiltProperties model =
    column
        [ padding 16
        , spacing 8
        , width fill
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
        ]


propertiesSpacer : Element Msg
propertiesSpacer =
    el
        [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , width fill
        , Border.color (rgb 0.5 0.5 0.5)
        ]
        none


canvasScale =
    100


viewDesigner : Model -> List (Svg Msg) -> Html Msg
viewDesigner model shapes =
    let
        widthStr : String
        widthStr =
            model.quiltWidth
                |> Tuple.second
                |> Length.inMeters
                |> (*) canvasScale
                |> (*) model.zoom
                |> String.fromFloat

        heightStr : String
        heightStr =
            model.quiltHeight
                |> Tuple.second
                |> Length.inMeters
                |> (*) canvasScale
                |> (*) model.zoom
                |> String.fromFloat
    in
    Svg.svg
        [ Svg.Attributes.width widthStr
        , Svg.Attributes.height heightStr
        , Svg.Attributes.viewBox (String.join " " [ "0", "0", widthStr, heightStr ])
        , Html.Attributes.style "border" "1px solid black"
        , Svg.Events.on "click" decodeSvgClick
        ]
        shapes


decodeSvgClick : Decoder Msg
decodeSvgClick =
    Json.Decode.map2 SvgMouseDown
        (Json.Decode.at [ "layerX" ] Json.Decode.float)
        (Json.Decode.at [ "layerY" ] Json.Decode.float)
