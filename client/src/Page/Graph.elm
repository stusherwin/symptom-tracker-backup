module Page.Graph exposing (Model, Msg(..), init, update, view)

import Colour exposing (Colour)
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode
import Maybe exposing (Maybe)
import Time exposing (Month(..))
import Trackable exposing (Trackable, TrackableData(..))
import Trackables exposing (Trackables)


type alias ScrollData =
    { scroll : Float
    , pcClientLeft : Float
    , pcClientRight : Float
    }


type alias Model =
    { today : Date
    , graph : Graph.Model
    , graphScroll : ScrollData
    }


init : Date -> Trackables -> Model
init today trackables =
    { today = today
    , graph = initGraph today trackables
    , graphScroll = { scroll = 100.0, pcClientLeft = 100, pcClientRight = 0 }
    }


initGraph : Date -> Trackables -> Graph.Model
initGraph today trackables =
    let
        dataSet : ( Int, Trackable ) -> Maybe ( Int, Graph.DataSet )
        dataSet ( tId, { question, colour, multiplier, data } ) =
            case data of
                TYesNo answers ->
                    Just
                        ( tId
                        , { name = question
                          , colour = colour
                          , multiplier = multiplier
                          , dataPoints =
                                List.map
                                    (Tuple.mapBoth Date.fromRataDie
                                        (\v ->
                                            if v then
                                                1

                                            else
                                                0
                                        )
                                    )
                                <|
                                    Dict.toList answers
                          }
                        )

                TIcon _ answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie toFloat) <| Dict.toList answers } )

                TScale _ _ answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie toFloat) <| Dict.toList answers } )

                TInt answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie toFloat) <| Dict.toList answers } )

                TFloat answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie identity) <| Dict.toList answers } )

                TText _ ->
                    Nothing
    in
    { today = today
    , data = concatMaybes <| List.map dataSet <| Trackables.toList trackables
    , selectedPoint = Nothing
    }



-- UPDATE


type Msg
    = GraphMsg Graph.Msg
    | Scroll ScrollData
    | TrackablesChanged Trackables


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GraphMsg graphMsg ->
            ( { model | graph = Graph.update graphMsg model.graph }, Cmd.none )

        Scroll graphScroll ->
            ( { model | graphScroll = graphScroll }, Cmd.none )

        TrackablesChanged trackables ->
            ( { model | graph = initGraph model.today trackables }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "shadow-inner-t-md" ]
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
        , div [ class "mx-4 my-0 flex", style "height" "300px" ]
            [ viewJustYAxis "flex-grow-0 flex-shrink-0" model.graph
            , viewScrollableContainer Scroll model.graphScroll [ Html.map GraphMsg <| viewLineGraph "h-full" model.graph ]
            ]
        ]


viewScrollableContainer : (ScrollData -> msg) -> ScrollData -> List (Html msg) -> Html msg
viewScrollableContainer toMsg scrollData children =
    div [ class "relative flex-grow" ]
        -- overflow-y-hidden" ]
        [ node "scrollable-container"
            [ class "absolute overflow-x-scroll top-0 bottom-scrollbar"
            , attribute "scroll" "100"
            , on "scrollable-scroll" (decodeScrollEvent toMsg)
            ]
            children

        -- , div
        --     [ class "absolute top-0 bottom-0 left-0 w-1/6 bg-gradient-white-transparent pointer-events-none"
        --     , style "opacity" <| String.fromFloat <| Basics.min 1.0 <| scrollData.pcClientLeft / (100 / 6)
        --     ]
        --     []
        -- , div
        --     [ class "absolute top-0 bottom-0 right-0 w-1/6 bg-gradient-transparent-white pointer-events-none"
        --     , style "opacity" <| String.fromFloat <| Basics.min 1.0 <| scrollData.pcClientRight / (100 / 6)
        --     ]
        --     []
        ]


decodeScrollEvent : (ScrollData -> msg) -> Decode.Decoder msg
decodeScrollEvent toMsg =
    let
        scrollLeft =
            Decode.field "target" (Decode.field "scrollLeft" Decode.float)

        scrollWidth =
            Decode.field "target" (Decode.field "scrollWidth" Decode.float)

        clientWidth =
            Decode.field "target" (Decode.field "clientWidth" Decode.float)

        calculate sl sw cw =
            toMsg
                { scroll = sl / (sw - cw) * 100
                , pcClientLeft = sl / cw * 100
                , pcClientRight = ((sw - cw) - sl) / cw * 100
                }
    in
    Decode.map3 calculate scrollLeft scrollWidth clientWidth


concatMaybes : List (Maybe a) -> List a
concatMaybes maybeXs =
    case maybeXs of
        (Just x) :: xs ->
            x :: concatMaybes xs

        _ :: xs ->
            concatMaybes xs

        _ ->
            []
