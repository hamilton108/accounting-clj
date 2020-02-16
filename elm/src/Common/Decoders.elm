module Common.Decoders exposing
    ( jsonStatusDecoder
    , selectItemDecoder
    , selectItemListDecoder
    )

import Common.Select exposing (SelectItem)
import Critters.Types exposing (JsonStatus)
import Json.Decode as JD
import Json.Decode.Pipeline as JP


jsonStatusDecoder : JD.Decoder JsonStatus
jsonStatusDecoder =
    JD.succeed JsonStatus
        |> JP.required "ok" JD.bool
        |> JP.required "msg" JD.string
        |> JP.required "statusCode" JD.int


selectItemDecoder : JD.Decoder SelectItem
selectItemDecoder =
    JD.map2
        SelectItem
        (JD.field "v" JD.string)
        (JD.field "t" JD.string)


selectItemListDecoder : JD.Decoder (List SelectItem)
selectItemListDecoder =
    JD.list selectItemDecoder
