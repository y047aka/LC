module Main exposing (main)

import Browser
import Html exposing (Html, a, br, button, div, h2, h3, h4, input, label, li, node, p, section, span, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Iso8601
import Launches exposing (Launch, Spacecraft, getTestServerResponseWithPageTask)
import Task
import Time exposing (Month(..), now)
import Time.Extra as Time exposing (Interval(..))
import View


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { userState : UserState
    , resultChunk : List Spacecraft
    , zone : Time.Zone
    , time : Time.Posix
    }


type UserState
    = Init
    | Loaded (List Launch)
    | Failed Http.Error


type Weekend
    = Scheduled Launch
    | Free
    | Past


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Init [] Time.utc (Time.millisToPosix 0)
    , qqq
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | GotServerResponse (Result Http.Error (List Spacecraft))
    | Recieve (Result Http.Error (List Launch))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        GotServerResponse (Ok categories) ->
            ( { model | resultChunk = categories }, Cmd.none )

        GotServerResponse (Err error) ->
            ( { model | resultChunk = [] }, Cmd.none )

        Recieve (Ok races) ->
            ( { model | userState = Loaded races }, Cmd.none )

        Recieve (Err error) ->
            ( { model | userState = Failed error }, Cmd.none )


qqq : Cmd Msg
qqq =
    let
        getResultTask =
            getTestServerResponseWithPageTask
    in
    Task.attempt GotServerResponse <|
        ([ "MHI/H-IIA.json"
         , "MHI/H-IIB.json"
         , "JAXA/Epsilon.json"
         , "IST/MOMO.json"
         , "SpaceX/Falcon9.json"
         ]
            |> List.map getResultTask
            |> Task.sequence
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "SpaceLaunchCalendar 2019"
    , body =
        [ View.siteHeader
        , node "main"
            []
            [ section []
                [ h2 [] [ text "Schedules" ]
                , div [] (model.resultChunk |> List.map (viewLaunchSchedule model))
                ]
            , section []
                [ h2 [] [ text "Archivements" ]
                , div [] (model.resultChunk |> List.map (viewHeatMap model))
                ]
            ]
        , View.siteFooter
        ]
    }


viewHeatMap : Model -> Spacecraft -> Html Msg
viewHeatMap model d =
    let
        utc =
            Time.utc

        start =
            Time.Parts 2013 Jan 1 0 0 0 0 |> Time.partsToPosix utc

        until =
            start |> Time.add Year 7 utc

        sundays =
            Time.range Sunday 1 utc start until
    in
    ul [ class "heatmap" ]
        [ h3 [] [ text d.seriesName ]
        , thead []
            [ tr []
                ([ "2013", "", "", "", "", "", "", "", "", "", "2014", "", "", "", "", "", "", "", "", "", "2015", "", "", "", "", "", "", "", "", "", "", "2016", "", "", "", "", "", "", "", "", "", "2017", "", "", "", "", "", "", "", "", "", "", "2018", "", "", "", "", "", "", "", "", "", "2019", "", "", "", "", "", "", "", "", "", "" ]
                    |> List.map (\posix -> th [] [ text posix ])
                )
            ]
        , li []
            (sundays
                |> List.map
                    (\sundayPosix ->
                        let
                            weekend =
                                isLaunchWeek sundayPosix d.launches model.time
                        in
                        viewHeatCell weekend
                    )
            )
        ]


viewHeatCell : Weekend -> Html Msg
viewHeatCell weekend =
    case weekend of
        Scheduled launch ->
            td [ class "raceweek" ]
                [ label []
                    [ input [ type_ "checkbox" ] []
                    , div []
                        [ text (launch.posix |> Iso8601.fromTime |> String.left 10)
                        , br [] []
                        , text launch.name
                        ]
                    ]
                ]

        Free ->
            td [] []

        Past ->
            td [ class "past" ] []


viewLaunchSchedule : Model -> Spacecraft -> Html Msg
viewLaunchSchedule model d =
    div [ class "heatmap" ]
        [ h3 [] [ text d.seriesName ]
        , let
            isFuture launch =
                Time.diff Day Time.utc model.time launch.posix > 0
          in
          ul []
            (d.launches
                |> List.filter isFuture
                |> List.map
                    (\launch ->
                        li []
                            [ a []
                                [ h4 [] [ text (launch.posix |> Iso8601.fromTime |> String.left 10) ]
                                , text launch.name
                                ]
                            ]
                    )
            )
        ]


isLaunchWeek : Time.Posix -> List Launch -> Time.Posix -> Weekend
isLaunchWeek sundayPosix races currentPosix =
    let
        racesInThisWeek =
            races
                |> List.filter
                    (\raceday ->
                        let
                            racedayPosix =
                                raceday.posix

                            diff =
                                Time.diff Day Time.utc racedayPosix sundayPosix
                        in
                        diff >= 0 && diff < 7
                    )

        isPast =
            Time.diff Day Time.utc sundayPosix currentPosix > 0
    in
    if List.length racesInThisWeek > 0 then
        Scheduled
            (racesInThisWeek
                |> List.reverse
                |> List.head
                |> Maybe.withDefault { name = "name", posix = Time.millisToPosix 0 }
            )

    else if isPast then
        Past

    else
        Free
