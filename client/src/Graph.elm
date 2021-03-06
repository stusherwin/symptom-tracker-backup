module Graph exposing (DataPoint, DataSet, Model, Msg, update, updateColour, updateMultiplier, viewJustYAxis, viewLineGraph)

import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOut, onMouseOver)


type Msg
    = PointSelected (Maybe ( Int, DataPoint ))
    | DataSetSelected Int


type alias Model =
    { today : Date
    , data : List ( Int, DataSet )
    , selectedPoint : Maybe ( Int, DataPoint )
    }


type alias DataSet =
    { name : String
    , colour : Colour
    , dataPoints : List DataPoint
    , multiplier : Float
    }


type alias DataPoint =
    ( Date, Float )


update : Msg -> Model -> Model
update msg model =
    case msg of
        PointSelected (Just ( i, d )) ->
            let
                set =
                    List.filter (\( s, _ ) -> s == i) model.data

                rest =
                    List.filter (\( s, _ ) -> s /= i) model.data
            in
            { model | selectedPoint = Just ( i, d ), data = set ++ rest }

        PointSelected _ ->
            { model | selectedPoint = Nothing }

        DataSetSelected i ->
            let
                set =
                    List.filter (\( s, _ ) -> s == i) model.data

                rest =
                    List.filter (\( s, _ ) -> s /= i) model.data
            in
            { model | data = set ++ rest }


updateColour : Int -> Colour -> Model -> Model
updateColour dataSetId colour model =
    { model | data = Dict.toList <| Dict.update dataSetId (Maybe.map (\ds -> { ds | colour = colour })) <| Dict.fromList model.data }


updateMultiplier : Int -> Float -> Model -> Model
updateMultiplier dataSetId multiplier model =
    { model | data = Dict.toList <| Dict.update dataSetId (Maybe.map (\ds -> { ds | multiplier = multiplier })) <| Dict.fromList model.data }


type alias PlotPoint =
    { date : Date
    , dateRD : Int
    , value : Float
    , x : Float
    , y : Float
    }


type alias GraphVals =
    { longDash : Float
    , shortDash : Float
    , xStep : Float
    , yStep : Float
    , mt : Float
    , mb : Float
    , ml : Float
    , mr : Float
    }


v : GraphVals
v =
    { longDash = 5.0
    , shortDash = 3.0
    , xStep = 15.0
    , yStep = 30.0
    , mt = 20.0
    , mb = 26.0
    , ml = 16.0
    , mr = 16.0
    }


type alias LineDefn =
    { strokeCol : Colour
    , strokeLinecap : String
    , strokeDasharray : Maybe String
    , x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


axisLine : LineDefn
axisLine =
    { strokeCol = Black, strokeLinecap = "square", strokeDasharray = Nothing, x1 = 0, y1 = 0, x2 = 0, y2 = 0 }


dottedLine : LineDefn
dottedLine =
    { strokeCol = LightGray
    , strokeDasharray = Just "4"
    , strokeLinecap = "square"
    , x1 = 0
    , y1 = 0
    , x2 = 0
    , y2 = 0
    }


line_ : Float -> LineDefn -> Svg msg
line_ h l =
    line
        ([ class <| "stroke-" ++ Colour.toString l.strokeCol, strokeLinecap l.strokeLinecap, x1 (String.fromFloat l.x1), y1 (String.fromFloat (h - l.y1)), x2 (String.fromFloat l.x2), y2 (String.fromFloat (h - l.y2)) ]
            ++ (case l.strokeDasharray of
                    Just sda ->
                        [ strokeDasharray sda ]

                    Nothing ->
                        []
               )
        )
        []


type alias RectDefn =
    { fillCol : Colour
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


rect_ : Float -> RectDefn -> Svg msg
rect_ h r =
    rect [ class <| "fill-" ++ Colour.toString r.fillCol, x (String.fromFloat r.x), y (String.fromFloat (h - r.y - r.height)), width (String.fromFloat r.width), height (String.fromFloat r.height) ] []


type alias TextDefn =
    { x : Float
    , y : Float
    , dominantBaseline : String
    , textAnchor : String
    , text : String
    }


text__ : Float -> TextDefn -> Svg msg
text__ h t =
    text_ [ x (String.fromFloat t.x), y (String.fromFloat (h - t.y)), fontSize "10px", dominantBaseline t.dominantBaseline, textAnchor t.textAnchor ] [ text t.text ]


type alias PolyLineDefn msg =
    { strokeCol : Colour
    , strokeWidth : Float
    , strokeLinecap : String
    , strokeLinejoin : String
    , points : List ( Float, Float )
    , onMouseOver : Maybe msg
    }


polyLine_ : Float -> PolyLineDefn msg -> Svg msg
polyLine_ h p =
    polyline
        ([ fill "none"
         , class <| "stroke-" ++ Colour.toString p.strokeCol
         , strokeWidth (String.fromFloat p.strokeWidth)
         , strokeLinecap p.strokeLinecap
         , strokeLinejoin p.strokeLinejoin
         , points <| String.join " " <| List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (h - y)) p.points
         ]
            ++ (case p.onMouseOver of
                    Just msg ->
                        [ onMouseOver msg ]

                    _ ->
                        []
               )
        )
        []


type alias CircleDefn msg =
    { cx : Float
    , cy : Float
    , r : Float
    , fillCol : Colour
    , strokeCol : Colour
    , strokeWidth : Float
    , onMouseOver : Maybe msg
    , onMouseOut : Maybe msg
    }


circle_ : Float -> CircleDefn msg -> Svg msg
circle_ h c =
    circle
        ([ cx (String.fromFloat c.cx)
         , cy (String.fromFloat (h - c.cy))
         , r (String.fromFloat c.r)
         , class <| "fill-" ++ Colour.toString c.fillCol
         , class <| "stroke-" ++ Colour.toString c.strokeCol
         , strokeWidth (String.fromFloat c.strokeWidth)
         ]
            ++ (case c.onMouseOver of
                    Just msg ->
                        [ onMouseOver msg ]

                    _ ->
                        []
               )
            ++ (case c.onMouseOut of
                    Just msg ->
                        [ onMouseOut msg ]

                    _ ->
                        []
               )
        )
        []


viewBox_ : Float -> Float -> Attribute msg
viewBox_ w h =
    viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)


viewJustYAxis : String -> Model -> Svg msg
viewJustYAxis graphClass { data } =
    let
        maxValue =
            toFloat <| ceiling (findMaxValue data / 5) * 5

        valueStep =
            floor (maxValue / 5)

        ( w, h ) =
            ( v.ml + v.longDash + 1, v.mb + v.mt + v.yStep * 5 )

        ( minY, maxY ) =
            ( v.mb, h - v.mt )
    in
    svg
        [ viewBox_ w h, class graphClass ]
    <|
        line_ h { axisLine | x1 = w - 1, y1 = minY, x2 = w - 1, y2 = maxY }
            :: List.concatMap
                (\n ->
                    [ line_ h { axisLine | x1 = v.ml, y1 = minY + toFloat n * v.yStep, x2 = v.ml + v.longDash, y2 = minY + toFloat n * v.yStep }
                    , text__ h { x = v.ml - 5.0, y = minY + toFloat n * v.yStep, dominantBaseline = "middle", textAnchor = "end", text = String.fromInt (n * valueStep) }
                    ]
                )
                (List.range 0 5)


viewLineGraph : String -> Model -> Svg Msg
viewLineGraph graphClass { data, today, selectedPoint } =
    let
        startDate =
            findStartDate today data

        maxValue =
            toFloat <| ceiling (findMaxValue data / 5) * 5

        valueStep =
            floor (maxValue / 5)

        startDateRD =
            Date.toRataDie startDate

        dayLength =
            Date.toRataDie (Date.add Days 1 startDate) - startDateRD

        xSteps =
            floor (toFloat (Date.toRataDie today - startDateRD) / toFloat dayLength)

        ( w, h ) =
            ( {- v.ml + v.mr + -} v.xStep * toFloat xSteps, v.mb + v.mt + v.yStep * 5 )

        minX =
            0

        --v.ml
        ( minY, maxY ) =
            ( v.mb, h - v.mt )

        xAxis =
            List.map (\n -> line_ h { dottedLine | x1 = minX + toFloat n * v.xStep, y1 = minY, x2 = minX + toFloat n * v.xStep, y2 = maxY })
                (List.range 0 xSteps)
                ++ line_ h { axisLine | x1 = 0, y1 = minY, x2 = w, y2 = minY }
                :: List.concatMap
                    (\n ->
                        if n == 0 then
                            [ line_ h { axisLine | x1 = minX + toFloat n * v.xStep + 1, y1 = minY, x2 = minX + toFloat n * v.xStep + 1, y2 = minY - v.longDash }
                            , text__ h { x = minX + toFloat n * v.xStep, y = minY - v.longDash - 5.0, dominantBaseline = "hanging", textAnchor = "start", text = Date.format "d MMM" <| Date.add Days n startDate }
                            ]

                        else if n == xSteps then
                            [ line_ h { axisLine | x1 = minX + toFloat n * v.xStep - 1, y1 = minY, x2 = minX + toFloat n * v.xStep - 1, y2 = minY - v.longDash }
                            , text__ h { x = minX + toFloat n * v.xStep, y = minY - v.longDash - 5.0, dominantBaseline = "hanging", textAnchor = "end", text = Date.format "d MMM" <| Date.add Days n startDate }
                            ]

                        else if xSteps <= 7 || n == xSteps || modBy 7 n == 0 then
                            [ line_ h { axisLine | x1 = minX + toFloat n * v.xStep, y1 = minY, x2 = minX + toFloat n * v.xStep, y2 = minY - v.longDash }
                            , text__ h { x = minX + toFloat n * v.xStep, y = minY - v.longDash - 5.0, dominantBaseline = "hanging", textAnchor = "middle", text = Date.format "d MMM" <| Date.add Days n startDate }
                            ]

                        else
                            [ line_ h { axisLine | x1 = minX + toFloat n * v.xStep, y1 = minY, x2 = minX + toFloat n * v.xStep, y2 = minY - v.shortDash } ]
                    )
                    (List.range 0 xSteps)

        yAxis =
            List.map
                (\n -> line_ h { dottedLine | x1 = 0, y1 = minY + toFloat n * v.yStep, x2 = w, y2 = minY + toFloat n * v.yStep })
                (List.range 1 5)

        axes =
            rect_ h
                { fillCol = LighterGray
                , x = 0
                , y = minY
                , width = w
                , height = maxY - minY
                }
                :: xAxis
                ++ yAxis

        dataLine : ( Int, DataSet ) -> List (Svg Msg)
        dataLine ( i, dataSet ) =
            let
                plotPoints : List PlotPoint
                plotPoints =
                    List.sortBy .dateRD <|
                        List.map
                            (\( date, value ) ->
                                let
                                    dateRD =
                                        Date.toRataDie date
                                in
                                { date = date
                                , value = value
                                , dateRD = dateRD
                                , x = minX + toFloat (dateRD - startDateRD) * v.xStep
                                , y = minY + ((value * dataSet.multiplier / toFloat valueStep) * v.yStep)
                                }
                            )
                            dataSet.dataPoints
            in
            -- polyLine_ h
            --     { strokeCol = White
            --     , strokeWidth = 3
            --     , strokeLinecap = "round"
            --     , strokeLinejoin = "round"
            --     , points = List.map (\{ x, y } -> ( x, y )) plotPoints
            --     , onMouseOver = Just (DataSetSelected i)
            --     }
            --     :: List.map
            --         (\{ date, value, x, y } ->
            --             circle_ h
            --                 { cx = x
            --                 , cy = y
            --                 , r =
            --                     if selectedPoint == Just ( i, ( date, value ) ) then
            --                         5
            --                     else
            --                         4
            --                 , fillCol = White
            --                 , strokeCol = White
            --                 , strokeWidth = 1
            --                 , onMouseOver = Just <| PointSelected <| Just ( i, ( date, value ) )
            --                 , onMouseOut = Just <| PointSelected Nothing
            --                 }
            --         )
            --         plotPoints
            --     ++
            [ polyLine_ h
                { strokeCol = dataSet.colour
                , strokeWidth = 2
                , strokeLinecap = "round"
                , strokeLinejoin = "round"
                , points = List.map (\{ x, y } -> ( x, y )) plotPoints
                , onMouseOver = Just (DataSetSelected i)
                }
            ]

        -- :: List.map
        --     (\{ date, value, x, y } ->
        --         circle_ h
        --             { cx = x
        --             , cy = y
        --             , r =
        --                 if selectedPoint == Just ( i, ( date, value ) ) then
        --                     4
        --                 else
        --                     3
        --             , fillCol = White
        --             , strokeCol = dataSet.colour
        --             , strokeWidth = 1
        --             , onMouseOver = Just <| PointSelected <| Just ( i, ( date, value ) )
        --             , onMouseOut = Just <| PointSelected Nothing
        --             }
        --     )
        --     plotPoints
    in
    svg [ viewBox_ w h, class graphClass ] <|
        axes
            ++ (List.concatMap dataLine <|
                    List.reverse data
               )


findStartDate : Date -> List ( Int, DataSet ) -> Date
findStartDate today data =
    let
        minDate =
            Date.fromRataDie
                << Maybe.withDefault (Date.toRataDie today)
                << List.minimum
                << List.map Date.toRataDie
                << concatMaybes
                << List.map (List.head << List.map Tuple.first << .dataPoints << Tuple.second)
            <|
                data

        fullWeeks =
            ceiling <| toFloat (Date.diff Days minDate today) / 7
    in
    Date.add Weeks -fullWeeks today


findMaxValue : List ( Int, DataSet ) -> Float
findMaxValue =
    Maybe.withDefault 0
        << List.maximum
        << concatMaybes
        << List.map (\{ multiplier, dataPoints } -> List.maximum <| List.map ((\val -> val * multiplier) << Tuple.second) dataPoints)
        << List.map Tuple.second


concatMaybes : List (Maybe a) -> List a
concatMaybes maybeXs =
    case maybeXs of
        (Just x) :: xs ->
            x :: concatMaybes xs

        _ :: xs ->
            concatMaybes xs

        _ ->
            []
