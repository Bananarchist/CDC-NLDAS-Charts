module Main exposing (main)

import Browser
import Data exposing (Datum, stateData)
import Html as Tag exposing (text)
import Html.Attributes as Hats
import Array
import Html.Events as Ev
import Html.Events.Extra
import Json.Decode as D
import Json.Encode as E
import LineChart
import LineChart.Colors
import LineChart.Dots
import List.Extra


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


type Year
    = Start Int
    | End Int


getYearValue year =
    case year of
        Start when ->
            when

        End when ->
            when


type alias Range =
    ( Float, Year )


states =
    List.Extra.uniqueBy .state stateData


init : () -> ( Model, Cmd Msg )
init _ =
    Model [ "Alabama" ] 1995 2005
        |> withoutCmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


withoutCmd model =
    ( model, Cmd.none )


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


view : Model -> Tag.Html Msg
view model =
    Tag.main_ []
        [ headerView
        , controlPanelView model
        , chartView model
        , footerView
        ]


headerView =
    Tag.header []
        [ Tag.h1 [] [ text "Sunlight Data 1973-2011" ]
        , Tag.article []
            [ Tag.p [] [ text "Using data from the CDC, this page graphs recorded sunlight exposure in KJ/m^2" ]
            ]
        ]


footerView =
    Tag.footer []
        [ text "Copyright someone or other 2021, data from the CDC" ]


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
            [ Tag.label [] [ text "Start" ]
            , Tag.input
                [ Hats.type_ "number"
                , Hats.min "1973"
                , Hats.max "2010"
                , Hats.value (String.fromFloat model.startYear)
                , Ev.onInput (String.toInt >> Maybe.withDefault 1973 >> UpdateStartYear)
                ]
                []
            , Tag.label [] [ text "End" ]
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
        [ text datum.state ]


colorOption  =
    Array.fromList [ LineChart.Colors.pink
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

chartView : Model -> Tag.Html Msg
chartView model =
    let
        filtering : Datum -> Bool
        filtering s =
            (s.year <= model.endYear)
                && (s.year >= model.startYear)
                && List.member s.state model.selectedStates

        filteredGroupedStates =
            stateData
                |> List.filter filtering
                |> List.Extra.gatherEqualsBy .state

    in
    Tag.div []
        [ LineChart.view .year
            .sunlight
            (List.indexedMap (\i ( n, s ) -> LineChart.line (color i) LineChart.Dots.circle n.state s) filteredGroupedStates)
        ]
