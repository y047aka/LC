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
            [ launchSchedules model
            , section []
                [ h2 [] [ text "Archivements" ]
                , let
                    utc =
                        Time.utc

                    start =
                        Time.Parts 2016 Jan 1 0 0 0 0 |> Time.partsToPosix utc

                    until =
                        start |> Time.add Year 4 utc

                    sundays =
                        Time.range Sunday 1 utc start until
                  in
                  div []
                    (model.resultChunk
                        |> List.map
                            (\d ->
                                ul [ class "heatmap" ]
                                    [ h3 [] [ text d.seriesName ]
                                    , tableHeader
                                    , tableBody sundays d.launches model.time
                                    ]
                            )
                    )
                ]
            ]
        , View.siteFooter
        ]
    }


tableHeader : Html Msg
tableHeader =
    thead []
        [ tr []
            ([ "2016", "", "", "", "", "", "", "", "", "", "", "", "", "2017", "", "", "", "", "", "", "", "", "", "", "", "", "2018", "", "", "", "", "", "", "", "", "", "", "", "", "2019", "", "", "", "", "", "", "", "", "", "", "" ]
                |> List.map (\posix -> th [] [ text posix ])
            )
        ]


type Weekend
    = Scheduled Launch
    | Free
    | Past


isRaceWeek : Time.Posix -> List Launch -> Time.Posix -> Weekend
isRaceWeek sundayPosix races currentPosix =
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


tableBody : List Time.Posix -> List Launch -> Time.Posix -> Html Msg
tableBody sundays races currentPosix =
    li []
        (sundays
            |> List.map
                (\sundayPosix ->
                    case isRaceWeek sundayPosix races currentPosix of
                        Scheduled race ->
                            td [ class "raceweek" ]
                                [ label []
                                    [ input [ type_ "checkbox" ] []
                                    , div []
                                        [ text (race.posix |> Iso8601.fromTime |> String.left 10)
                                        , br [] []
                                        , text race.name
                                        ]
                                    ]
                                ]

                        Free ->
                            td [] []

                        Past ->
                            td [ class "past" ] []
                )
        )


launchSchedules : Model -> Html Msg
launchSchedules model =
    section []
        [ h2 [] [ text "Schedules" ]
        , div []
            (model.resultChunk
                |> List.map
                    (\d ->
                        div [ class "heatmap" ]
                            [ h3 [] [ text d.seriesName ]
                            , ul []
                                (d.launches
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
                    )
            )
        ]
