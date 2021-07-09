module Main exposing (main)

import Array
import Axis
import Browser
import Data exposing (Datum, stateData)
import Html as Tag exposing (text)
import Html.Attributes as Hats
import Shape
import Color exposing (Color)
import Path exposing (Path)
import Html.Events as Ev
import Html.Events.Extra
import Json.Decode as D
import Json.Encode as E
import TypedSvg.Events
import List.Extra
import Scale exposing (ContinuousScale, OrdinalScale)
import Statistics
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)


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



stateButtonEvent : String -> Bool -> Tag.Attribute Msg
stateButtonEvent name selected =
    TypedSvg.Events.onClick (if selected then RemoveState name else SelectState name)

states : List Data.Datum
states =
    List.Extra.uniqueBy .state stateData


init : () -> ( Model, Cmd Msg )
init _ =
    Model [ "Alabama" ] 1995 2005
        |> withoutCmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


withoutCmd : Model -> (Model, Cmd Msg)
withoutCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
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
            model |> withoutCmd

        RemoveState state ->
            model |> withoutCmd

view : Model -> Tag.Html Msg
view model =
    Tag.main_ []
        [ headerView
        , controlPanelView model
        , vizView model

        --, chartView model
        , footerView
        ]


headerView : Tag.Html Msg
headerView =
    Tag.header []
        [ Tag.h1 [] [ Tag.text "Sunlight Data 1973-2011" ]
        , Tag.article []
            [ Tag.p [] [ Tag.text "Using data from the CDC, this page graphs recorded sunlight exposure in KJ/m^2" ]
            ]
        ]


footerView : Tag.Html Msg
footerView =
    Tag.footer []
        [ Tag.text "Copyright someone or other 2021, data from the CDC" ]


controlPanelView : Model -> Tag.Html Msg
controlPanelView model =
    Tag.section []
        [ stateSelectorView model
        , yearInput model
        ]


yearInput : Model -> Tag.Html Msg
yearInput model =
    Tag.fieldset []
        [ Tag.span []
            [ Tag.label [] [ Tag.text "Start" ]
            , Tag.input
                [ Hats.type_ "number"
                , Hats.min "1973"
                , Hats.max "2010"
                , Hats.value (String.fromFloat model.startYear)
                , Ev.onInput (String.toInt >> Maybe.withDefault 1973 >> UpdateStartYear)
                ]
                []
            , Tag.label [] [ Tag.text "End" ]
            , Tag.input
                [ Hats.type_ "number"
                , Hats.min "1974"
                , Hats.max "2011"
                , Hats.value (String.fromFloat model.endYear)
                , Ev.onInput (String.toInt >> Maybe.withDefault 2011 >> UpdateEndYear)
                ]
                []
            ]
        ]


stateSelectorView : Model -> Tag.Html Msg
stateSelectorView model =
    let
        opt =
            stateDatumOptionView model
    in
    Tag.select
        [ Ev.on "change" (D.map UpdateStates Html.Events.Extra.targetSelectedOptions)

        --[ Html.Events.Extra.onChange (Debug.log "target.value" >> D.decodeString (D.list D.string) >> Result.withDefault [ "Alabama" ] >> UpdateStates)
        --[ Ev.onInput (D.decodeString (D.list D.string) >> Result.withDefault [ "Alabama" ] >> UpdateStates)
        , Hats.value (model.selectedStates |> E.list E.string |> E.encode 0)
        , Hats.multiple True
        ]
        (List.map opt states)


stateDatumOptionView : Model -> Datum -> Tag.Html Msg
stateDatumOptionView model datum =
    Tag.option
        [ Hats.value datum.state
        , Hats.selected (List.any ((==) datum.state) model.selectedStates)
        ]
        [ Tag.text datum.state ]


colorOption =
    Array.fromList
        [ LineChart.Colors.pink
        , LineChart.Colors.blue
        , LineChart.Colors.gold
        , LineChart.Colors.red
        , LineChart.Colors.green
        , LineChart.Colors.cyan
        , LineChart.Colors.purple
        , LineChart.Colors.teal
        , LineChart.Colors.rust
        , LineChart.Colors.strongBlue
        , LineChart.Colors.pinkLight
        , LineChart.Colors.blueLight
        , LineChart.Colors.goldLight
        , LineChart.Colors.redLight
        , LineChart.Colors.greenLight
        , LineChart.Colors.cyanLight
        , LineChart.Colors.tealLight
        , LineChart.Colors.purpleLight
        , LineChart.Colors.black
        , LineChart.Colors.gray
        , LineChart.Colors.grayLight
        , LineChart.Colors.grayLightest
        ]


color idx =
    Array.get idx colorOption
        |> Maybe.withDefault LineChart.Colors.pink



w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60



filteredAndGroupedStaetData : Model -> List (List Datum)
filteredAndGroupedStaetData model =
    let
        filterFn s =
            (s.year <= model.endYear)
                && (s.year >= model.startYear)
                && List.member s.state model.selectedStates
    in
    stateData
        |> List.filter filterFn
        |> List.Extra.gatherEqualsBy .state
        |> List.map Tuple.second


vizView : Model -> Tag.Html Msg
vizView model =
    let
        data =
            filteredAndGroupedStaetData model

        flatData =
            List.concat data

        sunExtent =
            flatData
            |> List.map .sunlight
            |> Statistics.extent
            |> Maybe.map (Tuple.second >> Tuple.pair 0)
            |> Maybe.withDefault (0, 1000000)
            |> Debug.log "sunExtent"

        preExtent =
            flatData
            |> List.map .precipitation
            |> Statistics.extent
            |> Maybe.map (Tuple.second >> Tuple.pair 0)
            |> Maybe.withDefault (0, 100)


        xScale : ContinuousScale Float
        xScale =
            Scale.linear ( 0, w - 2 * padding ) ( model.startYear, model.endYear )

        yScaleDomain =
            Scale.linear ( h - 2 * padding, 0 )

        yScaleSun : ContinuousScale Float
        yScaleSun =
            sunExtent
                |> yScaleDomain
                |> Scale.nice 4
                |> Debug.log "yScaleSun"

        yScalePre : ContinuousScale Float
        yScalePre =
            preExtent
                |> yScaleDomain
                |> Scale.nice 4
                |> Debug.log "yScalePre"

        xAxis : Svg Msg
        xAxis =
            g [ transform [ Translate (padding - 1) (h - padding) ] ]
                [ Axis.bottom [ Axis.tickCount 10 ] xScale ]

        yAxisSun =
            g [ transform [ Translate (padding - 1) padding ] ]
                [ Axis.left [ Axis.tickCount 10 ] yScaleSun ]

        yAxisPre =
            g [ transform [ Translate (w - padding - 1) padding ] ]
                [ Axis.left [ Axis.tickCount 10 ] yScalePre ]

        lineGenerator ysc (x,y) = 
            Just ( Scale.convert xScale (x), Scale.convert ysc (y))

        line ysc accessor mappableData =
            List.map  (\i -> (.year i, accessor i)) mappableData 
                |> List.map (lineGenerator ysc)
                |> Shape.line Shape.linearCurve
    in
    svg [ viewBox 0 0 w h ]
        [ xAxis
        , yAxisSun
        , yAxisPre
        , g [transform [ Translate padding padding ], class ["series"] ] <|
            List.map
                (\sData -> Path.element (line yScaleSun (.sunlight ) sData)
                    [ strokeWidth 3
                    , stroke <| Paint Color.yellow
                    , fill PaintNone
                    ])
                data 
        , g [transform [ Translate padding padding ], class ["series"] ] <|
            List.map
                (\sData -> Path.element (line yScalePre (.precipitation) sData )
                    [ strokeWidth 3
                    , stroke <| Paint Color.lightBlue
                    , fill PaintNone
                    ]
                )
                data
        ]

