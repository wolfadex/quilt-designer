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
import Length exposing (Length)
import LineSegment2d
import Point2d
import Svg
import Svg.Attributes
import Tooltip exposing (Dimensions, Position(..))


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
    , blocks : Array Block
    , activeBlock : Maybe Int
    }


type alias Block =
    { width : ( String, Length )
    , height : ( String, Length )
    , label : String
    , confirmDelete : Maybe Dimensions
    }



-- INIT


init : Model
init =
    { quiltWidth = ( "1", Length.meters 1 )
    , quiltHeight = ( "2", Length.meters 2 )
    , zoom = 1
    , blocks = Array.empty
    , activeBlock = Nothing
    }



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


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotActiveBlock activeBlock ->
            { model | activeBlock = activeBlock }

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

        CreateBlock ->
            { model
                | blocks =
                    Array.push
                        { width = ( "0.25", Length.meters 0.25 )
                        , height = ( "0.25", Length.meters 0.25 )
                        , label = ""
                        , confirmDelete = Nothing
                        }
                        model.blocks
            }

        GotBlockName index label ->
            { model
                | blocks =
                    Array.Extra.update index
                        (\block -> { block | label = label })
                        model.blocks
            }

        GotBlockWidth index widthStr ->
            { model
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

        PromptDeleteBlock index dimensions ->
            { model
                | blocks =
                    Array.Extra.update index
                        (\block -> { block | confirmDelete = Just dimensions })
                        model.blocks
            }

        CancelDeleteBlock index ->
            { model
                | blocks =
                    Array.Extra.update index
                        (\block -> { block | confirmDelete = Nothing })
                        model.blocks
            }

        ConfirmDeleteBlock index ->
            { model
                | blocks =
                    Array.append
                        (Array.slice 0 index model.blocks)
                        (Array.slice (index + 1) (Array.length model.blocks) model.blocks)
            }



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
            []
            [ column
                [ padding 16
                , spacing 8
                , width fill
                , alignTop
                ]
                [ text "Quilt"
                , quiltProperties model
                , propertiesSpacer
                , text "Blocks"
                , Input.button
                    [ paddingXY 16 8
                    , Border.width 3
                    , Border.color (rgb 0.5 0.7 0.6)
                    ]
                    { onPress = Just CreateBlock
                    , label = text "Create Block"
                    }
                , model.blocks
                    |> Array.toIndexedList
                    |> List.map viewBlockProperties
                    |> column [ spacing 8, padding 16 ]
                ]
            , column
                [ alignTop ]
                [ model.blocks
                    |> Array.toIndexedList
                    |> List.map (viewBlockTab model.activeBlock)
                    |> (::) (viewQuiltTab model.activeBlock)
                    |> List.intersperse tabSpacer
                    |> row []
                , viewDesigner model
                    |> html
                    |> el []
                ]
            ]
        ]


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


propertiesSpacer : Element Msg
propertiesSpacer =
    el
        [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , width fill
        , Border.color (rgb 0.5 0.5 0.5)
        ]
        none


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
