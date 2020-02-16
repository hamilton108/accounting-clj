module Common.Types exposing (JsonStatus, jsonStatusDecoder)

import Json.Decode as JD
import Json.Decode.Pipeline as JP


type alias JsonStatus =
    { ok : Bool, msg : String, statusCode : Int }


jsonStatusDecoder : JD.Decoder JsonStatus
jsonStatusDecoder =
    JD.succeed JsonStatus
        |> JP.required "ok" JD.bool
        |> JP.required "msg" JD.string
        |> JP.required "statusCode" JD.int
