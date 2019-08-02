module Main exposing (main)

import Array
import Browser exposing (Document)
import Dict
import Html exposing (Attribute, Html, div, input, label, node, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode
import LngLat exposing (LngLat)
import Mapbox.Element exposing (..)
import Mapbox.Expression as E exposing (..)
import Mapbox.Layer as Layer exposing (Layer)
import Mapbox.Source as Source exposing (Source)
import Mapbox.Style exposing (Style(..))
import Maybe
import Styles.Style exposing (mapStyle)
import Tuple


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "Wards" "Mayor" "5" False (LngLat 0 0) [] False
    , Cmd.none
    )


colors =
    Array.fromList [ "#d95f02", "#7570b3" ]


interpolationMargin =
    [ -0.6, -0.3, -0.01, 0.01, 0.3, 0.6 ]


interpolationColor =
    [ "#542788"
    , "#998ec3"
    , "#d8daeb"
    , "#fee0b6"
    , "#f1a340"
    , "#b35806"
    ]


interpolationColors =
    [ ( 84, 39, 136 )
    , ( 153, 142, 195 )
    , ( 216, 218, 235 )
    , ( 254, 224, 182 )
    , ( 241, 163, 64 )
    , ( 179, 88, 6 )
    ]


toColorStr : ( Float, Float, Float ) -> String
toColorStr ( r, g, b ) =
    "rgba(" ++ String.fromFloat r ++ "," ++ String.fromFloat g ++ "," ++ String.fromFloat b ++ ")"


type alias Race =
    String


races =
    [ "Mayor", "Treasurer", "City Council" ]


type alias Ward =
    String


wards =
    [ "5", "6", "15", "16", "20", "21", "25", "30", "31", "33", "39", "40", "43", "46", "47" ]


type alias MapLayer =
    String


layers =
    [ "Wards", "Precincts" ]


type alias Candidate =
    { key : String, label : String, color : String }


wardCandidateMap =
    Dict.fromList
        [ ( "Mayor", Dict.fromList [ ( "lori_lightfoot", "Lightfoot" ), ( "toni_preckwinkle", "Preckwinkle" ) ] )
        , ( "Treasurer", Dict.fromList [ ( "melissa_conyearservin", "Conyears-Ervin" ), ( "ameya_pawar", "Pawar" ) ] )
        , ( "5", Dict.fromList [ ( "leslie_a_hairston", "Hairston" ), ( "william_calloway", "Calloway" ) ] )
        , ( "6", Dict.fromList [ ( "roderick_t_sawyer", "Sawyer" ), ( "deborah_a_fosterbonner", "Foster-Bonner" ) ] )
        ]


getCandidates : Race -> Ward -> List Candidate
getCandidates race ward =
    case
        if race == "City Council" then
            Dict.get ward wardCandidateMap

        else
            Dict.get race wardCandidateMap
    of
        Just candidateDict ->
            Dict.toList candidateDict
                |> List.indexedMap (\idx ( k, v ) -> Candidate k v (Maybe.withDefault "#d95f02" (Array.get idx colors)))
                |> List.sortBy .label

        Nothing ->
            []


candidatesLegend : List Candidate -> List (Html Msg)
candidatesLegend candidates =
    List.concatMap
        (\candidate ->
            [ p []
                [ span [ attribute "class" "color", style "background-color" candidate.color ] []
                , span [ attribute "class" "label" ] [ text candidate.label ]
                ]
            , div [ attribute "class" "ramp-key" ] []
            ]
        )
        candidates


rampKey : Int -> List (Html Msg)
rampKey index =
    let
        labels =
            Array.fromList [ "50.1%", "65%", "80+%" ]

        keyItems =
            if index == 1 then
                interpolationColors |> List.take 3 |> List.reverse

            else
                interpolationColors |> List.drop 3
    in
    keyItems
        |> List.indexedMap (\idx color -> div [ attribute "class" "item", style "background-color" (toColorStr color) ] [ text (Maybe.withDefault "" (Array.get idx labels)) ])


popOpacityScale : Model -> E.Expression E.DataExpression Float
popOpacityScale model =
    if model.scaleOpacity then
        let
            maxScale =
                if model.layer == "Wards" then
                    17500

                else
                    400
        in
        E.getProperty (str "ballots_cast") |> E.interpolate E.Linear [ ( 0, float 0 ), ( maxScale, float 0.8 ) ]

    else
        E.conditionally [ ( E.getProperty (str "ballots_cast") |> E.isEqual (float 0), float 0 ) ] (float 0.8)


candidateFillColor : List Candidate -> E.Expression E.DataExpression E.Color
candidateFillColor candidates =
    case candidates of
        [ a, b ] ->
            (E.getProperty (str a.key) |> E.divideBy (E.getProperty (str "ballots_cast")))
                |> E.minus (E.getProperty (str b.key) |> E.divideBy (E.getProperty (str "ballots_cast")))
                |> E.interpolate E.Linear (List.map2 Tuple.pair interpolationMargin interpolationColors |> List.map (\( v, ( r, g, bl ) ) -> ( v, E.rgba r g bl 1 )))

        other ->
            E.getProperty (str "ballots_cast")


getLayer : Model -> String -> Layer
getLayer model layerStr =
    let
        candidates =
            getCandidates model.race model.ward

        candidateKey =
            case List.head candidates of
                Just candidate ->
                    candidate.key

                Nothing ->
                    ""
    in
    Layer.fill layerStr
        layerStr
        [ Layer.sourceLayer layerStr
        , Layer.filter
            (if String.toLower model.layer == layerStr then
                E.hasProperty (str candidateKey)

             else
                E.hasProperty (str "FAKEPROP")
            )
        , Layer.fillColor (candidateFillColor candidates)
        , Layer.fillOpacity (popOpacityScale model)
        ]


type alias Model =
    { layer : MapLayer, race : Race, ward : Ward, scaleOpacity : Bool, position : LngLat, features : List Json.Encode.Value, cursorPointer : Bool }


type Msg
    = UpdateLayer MapLayer
    | UpdateRace Race
    | UpdateWard Ward
    | UpdateOpacity Bool
    | Hover EventData
    | MouseOut EventData
    | Click EventData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLayer newLayer ->
            ( { model | layer = newLayer }, Cmd.none )

        UpdateRace newRace ->
            ( { model | race = newRace }, Cmd.none )

        UpdateWard newWard ->
            ( { model | ward = newWard }, Cmd.none )

        UpdateOpacity newOpacity ->
            ( { model | scaleOpacity = newOpacity }, Cmd.none )

        Hover { lngLat, renderedFeatures } ->
            ( { model | cursorPointer = Basics.not (List.isEmpty renderedFeatures), position = lngLat, features = renderedFeatures }, Cmd.none )

        MouseOut _ ->
            ( { model | cursorPointer = False }, Cmd.none )

        Click { lngLat, renderedFeatures } ->
            ( { model | position = lngLat, features = renderedFeatures }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "height" "100vh"
        ]
        [ node "style"
            []
            [ text
                ("elm-mapbox-map .mapboxgl-canvas-container.mapboxgl-interactive{cursor:"
                    ++ (if model.cursorPointer then
                            "pointer"

                        else
                            "''"
                       )
                    ++ ";}"
                )
            ]
        , Mapbox.Element.map
            [ minZoom 9
            , maxZoom 17
            , onMouseMove Hover
            , Mapbox.Element.onMouseOut MouseOut
            , eventFeaturesLayers [ "wards", "precincts" ]
            ]
            (mapStyle
                [ getLayer model "wards"
                , getLayer model "precincts"
                ]
            )
        , div [ attribute "id" "legend" ]
            ([ p [ attribute "class" "label" ] [ text "Chicago 2019 Runoff" ] ]
                ++ List.map
                    (\race ->
                        p []
                            [ label []
                                [ input
                                    [ type_ "radio"
                                    , name "results"
                                    , value race
                                    , checked (race == model.race)
                                    , onCheck (\b -> UpdateRace race)
                                    ]
                                    []
                                , text race
                                ]
                            ]
                    )
                    races
            )
        ]
