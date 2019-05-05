module Launches exposing (Launch, Spacecraft, getTestServerResponseWithPageTask)

import Http
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Task
import Time exposing (Month(..), now)



-- TYPES


type alias Spacecraft =
    { name : String
    , launches : List Launch
    }


type alias Launch =
    { posix : Time.Posix
    , name : String
    }



-- DECODER


raceCategoryDecoder : Decode.Decoder Spacecraft
raceCategoryDecoder =
    Decode.map2 Spacecraft
        (Decode.field "spacecraft" Decode.string)
        (Decode.field "launches" (Decode.list raceDecoder))


raceDecoder : Decode.Decoder Launch
raceDecoder =
    Decode.succeed Launch
        |> required "date" Iso8601.decoder
        |> required "name" Decode.string



-- API


getTestServerResponseWithPageTask : String -> Task.Task Http.Error Spacecraft
getTestServerResponseWithPageTask category =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://sorabatake.github.io/SpaceBusinessData/schedules/" ++ category
        , body = Http.emptyBody
        , resolver = jsonResolver raceCategoryDecoder
        , timeout = Nothing
        }


jsonResolver : Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))
