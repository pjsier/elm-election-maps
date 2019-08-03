module Main exposing (main)

import Array
import Browser exposing (Document)
import Cmd exposing (Point, projectPoint, projectedPoint)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, hr, input, label, node, option, p, select, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
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
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "Wards" "Mayor" "5" False (LngLat 0 0) (Point 0 0) False [] False
    , Cmd.none
    )


mobileBreakpoint =
    600


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
                [ span [ class "color", style "background-color" candidate.color ] []
                , span [ class "label" ] [ text candidate.label ]
                ]
            , div [ class "ramp-key" ] []
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
        |> List.indexedMap (\idx color -> div [ class "item", style "background-color" (toColorStr color) ] [ text (Maybe.withDefault "" (Array.get idx labels)) ])


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


type alias Feature =
    { ward : String, precinct : Maybe Int, ballots : Int, voters : Int, candidates : Dict String Int }


featDecoder : Decode.Decoder Feature
featDecoder =
    let
        candidateKeys =
            wardCandidateMap |> Dict.values |> List.map Dict.keys |> List.concat
    in
    Decode.map5 Feature
        (Decode.at [ "properties", "ward" ] (Decode.oneOf [ Decode.int |> Decode.map String.fromInt, Decode.string ]))
        (Decode.maybe (Decode.at [ "properties", "precinct" ] Decode.int))
        (Decode.at [ "properties", "ballots_cast" ] Decode.int)
        (Decode.at [ "properties", "registered_voters" ] Decode.int)
        (Decode.field "properties"
            (Decode.keyValuePairs (Decode.oneOf [ Decode.int, Decode.succeed 0 ])
                |> Decode.map (\vals -> List.filter (\( k, v ) -> List.member k candidateKeys) vals)
                |> Decode.map Dict.fromList
            )
        )


popupEl : Model -> List Candidate -> Html Msg
popupEl model candidates =
    case List.head model.features of
        Just featObj ->
            case Decode.decodeValue featDecoder featObj of
                Ok feat ->
                    popupElFeat model candidates feat

                Err _ ->
                    text ""

        Nothing ->
            text ""


popupElFeat : Model -> List Candidate -> Feature -> Html Msg
popupElFeat model candidates feat =
    let
        labelContent =
            model.race
                ++ ": Ward "
                ++ feat.ward
                ++ (if model.layer == "Precincts" then
                        ", Precinct " ++ String.fromInt (Maybe.withDefault 0 feat.precinct)

                    else
                        ""
                   )

        voteDenom =
            List.foldl (+) 0 (List.map (\c -> Maybe.withDefault 0 (Dict.get c.key feat.candidates)) candidates)
    in
    div
        [ class "mapboxgl-popup mapboxgl-popup-anchor-bottom"
        , style "transform" ("translate(-50%, -100%) translate(" ++ String.fromFloat model.point.x ++ "px," ++ String.fromFloat model.point.y ++ "px)")
        ]
        [ div [ class "mapboxgl-popup-tip" ] []
        , div [ class "mapboxgl-popup-content" ]
            ([ div [ class "popup-prop area" ] [ div [ class "popup-prop-name" ] [ text labelContent ] ]
             , div
                [ class "popup-prop area turnout" ]
                [ div [ class "popup-prop-name" ] [ text "Turnout" ]
                , div [ class "popup-prop-value" ] [ text (((Basics.toFloat feat.ballots / Basics.toFloat feat.voters) * 100 |> Basics.round |> String.fromInt) ++ "%") ]
                ]
             , div [ class "popup-prop area turnout" ]
                [ div [ class "popup-prop-name" ] [ text "Votes" ]
                , div [ class "popup-prop-value" ] [ text (String.fromInt voteDenom) ]
                ]
             ]
                ++ List.map
                    (\candidate ->
                        div [ class "popup-prop" ]
                            [ div
                                [ class "popup-prop-name" ]
                                [ span [ class "color", style "background-color" candidate.color ] []
                                , span [] [ text candidate.label ]
                                ]
                            , div
                                [ class "popup-prop-value" ]
                                [ div [] [ text (Dict.get candidate.key feat.candidates |> Maybe.withDefault 0 |> String.fromInt) ]
                                , div []
                                    [ text
                                        ((Dict.get candidate.key feat.candidates
                                            |> Maybe.withDefault 0
                                            |> Basics.toFloat
                                            |> (/) (Basics.toFloat voteDenom)
                                            |> (*) 100
                                            |> String.fromFloat
                                         )
                                            ++ "%"
                                        )
                                    ]
                                ]
                            ]
                    )
                    candidates
            )
        ]


type alias Model =
    { layer : MapLayer
    , race : Race
    , ward : Ward
    , scaleOpacity : Bool
    , position : LngLat
    , point : Point
    , showPopup : Bool
    , features : List Encode.Value
    , cursorPointer : Bool
    }


type Msg
    = UpdateLayer MapLayer
    | UpdateRace Race
    | UpdateWard Ward
    | UpdateOpacity Bool
    | Hover EventData
    | MouseOut EventData
    | Click EventData
    | ProjectedPoint Point


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
            let
                isActive =
                    Basics.not (List.isEmpty renderedFeatures)
            in
            ( { model | cursorPointer = isActive, showPopup = isActive, position = lngLat, features = renderedFeatures }, projectPoint lngLat )

        ProjectedPoint point ->
            ( { model | point = point }, Cmd.none )

        MouseOut _ ->
            ( { model | showPopup = True, cursorPointer = False }, Cmd.none )

        Click { lngLat, renderedFeatures } ->
            ( { model | position = lngLat, features = renderedFeatures }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    projectedPoint ProjectedPoint


view : Model -> Html Msg
view model =
    let
        candidates =
            getCandidates model.race model.ward
    in
    div
        []
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
            , Mapbox.Element.id "map"
            , eventFeaturesLayers [ "wards", "precincts" ]
            ]
            (mapStyle
                [ getLayer model "wards"
                , getLayer model "precincts"
                ]
            )
        , if model.showPopup then
            popupEl model candidates

          else
            text ""
        , div [ attribute "id" "legend" ]
            ([ p [ class "label" ] [ text "Chicago 2019 Runoff" ] ]
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
                ++ (if model.race == "City Council" then
                        [ p []
                            [ label
                                [ attribute "for" "council" ]
                                [ text "Ward" ]
                            , select
                                [ attribute "id" "council", name "council", onInput UpdateWard ]
                                (List.map (\ward -> option [ value ward, selected (model.ward == ward) ] [ text ward ]) wards)
                            ]
                        ]

                    else
                        []
                   )
                ++ [ hr [] [] ]
                ++ (List.indexedMap
                        (\idx c ->
                            [ p []
                                [ span [ class "color", style "background-color" c.color ] []
                                , span [ class "label" ] [ text c.label ]
                                ]
                            , div [ class "ramp-key" ] (rampKey idx)
                            ]
                        )
                        candidates
                        |> List.concat
                   )
                ++ [ hr [] [] ]
                ++ List.map (\l -> p [] [ label [] [ input [ type_ "radio", name "layer", value l, checked (model.layer == l), onCheck (\b -> UpdateLayer l) ] [], text l ] ]) layers
                ++ [ p [] [ label [] [ input [ type_ "checkbox", name "opacity", value "opacity", checked model.scaleOpacity, onCheck UpdateOpacity ] [], text "Opacity by # Votes" ] ] ]
            )
        ]
