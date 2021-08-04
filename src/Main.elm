module Main exposing (main)

import Axis
import Basics.Extra exposing (flip)
import Browser
import Color exposing (Color)
import Data exposing (Datum, stateData)
import Html as Tag
import Html.Attributes as Hats exposing (id)
import Html.Events as Ev
import List exposing (filter, length, member, take)
import List.Extra
import Path exposing (Path)
import Scale exposing (ContinuousScale, OrdinalScale)
import Shape
import Statistics
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)
import USA


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { selectedStates : List String
    , startYear : Float
    , endYear : Float
    }


type Msg
    = UpdateStates (List String)
    | UpdateStartYear Int
    | UpdateEndYear Int
    | SelectState String
    | RemoveState String


init : () -> ( Model, Cmd Msg )
init _ =
    Model [ "Alabama" ] 1995 2005
        |> withoutCmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


withoutCmd : Model -> ( Model, Cmd Msg )
withoutCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStates newStates ->
            { model | selectedStates = newStates }
                |> withoutCmd

        UpdateStartYear year ->
            { model | startYear = toFloat year }
                |> withoutCmd

        UpdateEndYear year ->
            { model | endYear = toFloat year }
                |> withoutCmd

        SelectState state ->
            model
                |> plusState state
                |> withoutCmd

        RemoveState state ->
            model
                |> minusState state
                |> withoutCmd


plusState : String -> Model -> Model
plusState state model =
    let
        newStates =
            if member state model.selectedStates then
                model.selectedStates

            else if length model.selectedStates == 5 then
                state :: take 4 model.selectedStates

            else
                state :: model.selectedStates
    in
    { model | selectedStates = newStates }


minusState : String -> Model -> Model
minusState state model =
    { model | selectedStates = filter ((/=) state) model.selectedStates }


view : Model -> Tag.Html Msg
view model =
    Tag.main_ []
        [ controlPanelView model
        , vizView model
        ]


controlPanelView : Model -> Tag.Html Msg
controlPanelView model =
    Tag.section [ id "control-panel" ]
        [ stateSelectorView model
        , yearInput model
        ]


yearInput : Model -> Tag.Html Msg
yearInput model =
    Tag.fieldset []
        [ Tag.input
            [ Hats.type_ "number"
            , Hats.min "1973"
            , Hats.max "2010"
            , Hats.value (String.fromFloat model.startYear)
            , Ev.onInput (String.toInt >> Maybe.withDefault 1973 >> UpdateStartYear)
            ]
            []
        , Tag.text "-"
        , Tag.input
            [ Hats.type_ "number"
            , Hats.min "1974"
            , Hats.max "2011"
            , Hats.value (String.fromFloat model.endYear)
            , Ev.onInput (String.toInt >> Maybe.withDefault 2011 >> UpdateEndYear)
            ]
            []
        ]


stateSelectorView : Model -> Tag.Html Msg
stateSelectorView model =
    let
        opt =
            stateDatumOptionView model
    in
    svg
        [ viewBox 0 0 1200 1000
        ]
        [ USA.states model.selectedStates SelectState RemoveState ]


stateDatumOptionView : Model -> Datum -> Tag.Html Msg
stateDatumOptionView model datum =
    Tag.option
        [ Hats.value datum.state
        , Hats.selected (List.any ((==) datum.state) model.selectedStates)
        ]
        [ Tag.text datum.state ]


w : Float
w =
    900


h : Float
h =
    450


xpad : Float
xpad =
    60


ypad : Float
ypad =
    30


filteredAndGroupedStateData : Model -> List (List Datum)
filteredAndGroupedStateData model =
    let
        yearFilter s =
            (s.year <= model.endYear) && (s.year >= model.startYear)
    in
    stateData
        |> List.filter (.state >> flip member model.selectedStates)
        |> List.filter yearFilter
        |> List.Extra.gatherEqualsBy .state
        |> List.map Tuple.second


vizView : Model -> Tag.Html Msg
vizView model =
    let
        data =
            filteredAndGroupedStateData model

        flatData =
            List.concat data

        sunExtent =
            flatData
                |> List.map .sunlight
                |> Statistics.extent
                |> Maybe.map (Tuple.second >> Tuple.pair 0)
                |> Maybe.withDefault ( 0, 1000000 )

        preExtent =
            flatData
                |> List.map .precipitation
                |> Statistics.extent
                |> Maybe.map (Tuple.second >> Tuple.pair 0)
                |> Maybe.withDefault ( 0, 100 )

        xScale : ContinuousScale Float
        xScale =
            Scale.linear ( 0, w - 2 * xpad ) ( model.startYear, model.endYear )

        yScaleDomain =
            Scale.linear ( h - 2 * ypad, 0 )

        yScaleSun : ContinuousScale Float
        yScaleSun =
            sunExtent
                |> yScaleDomain
                |> Scale.nice 4

        yScalePre : ContinuousScale Float
        yScalePre =
            preExtent
                |> yScaleDomain
                |> Scale.nice 4

        xAxis : Svg Msg
        xAxis =
            g [ transform [ Translate (xpad - 1) (h - ypad) ] ]
                [ Axis.bottom [ Axis.tickCount 10 ] xScale ]

        yAxisSun =
            g [ transform [ Translate (xpad - 1) ypad ] ]
                [ Axis.left [ Axis.tickCount 10 ] yScaleSun ]

        yAxisPre =
            g [ transform [ Translate (w - xpad - 1) ypad ] ]
                [ Axis.left [ Axis.tickCount 10 ] yScalePre ]

        lineGenerator ysc ( x, y ) =
            Just ( Scale.convert xScale x, Scale.convert ysc y )

        line ysc accessor mappableData =
            List.map (\i -> ( .year i, accessor i )) mappableData
                |> List.map (lineGenerator ysc)
                |> Shape.line Shape.linearCurve
    in
    svg
        [ viewBox 0 0 w h
        , id "viz-view"
        ]
        [ xAxis
        , yAxisSun
        , yAxisPre
        , g [ transform [ Translate xpad ypad ], class [ "series" ] ] <|
            List.map
                (\sData ->
                    Path.element (line yScaleSun .sunlight sData)
                        [ strokeWidth 3
                        , stroke <| Paint Color.yellow
                        , fill PaintNone
                        ]
                )
                data
        , g [ transform [ Translate xpad ypad ], class [ "series" ] ] <|
            List.map
                (\sData ->
                    Path.element (line yScalePre .precipitation sData)
                        [ strokeWidth 3
                        , stroke <| Paint Color.lightBlue
                        , fill PaintNone
                        ]
                )
                data
        ]
