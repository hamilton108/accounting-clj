module Common.DateUtil exposing
    ( UnixTime
    , dateRangeOf
    , day_
    , unixTimeStr
    )

import Common.Utils as U
import Time


type alias UnixTime =
    Int


day_ : UnixTime
day_ =
    86400000


dateRangeOf : UnixTime -> List UnixTime -> ( UnixTime, UnixTime )
dateRangeOf dx lx =
    let
        offsetHi =
            List.head lx |> Maybe.withDefault 0

        offsetLow =
            U.lastElem lx |> Maybe.withDefault 0
    in
    -- ( dxOf dx offsetLow, dxOf dx offsetHi )
    ( dx + (offsetLow * day_), dx + (offsetHi * day_) )


monthStr : Time.Month -> String
monthStr month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


unixTimeStr : UnixTime -> String
unixTimeStr tm =
    let
        pt =
            Time.millisToPosix tm

        y =
            String.fromInt <| Time.toYear Time.utc pt

        mo =
            monthStr <| Time.toMonth Time.utc pt

        d =
            String.fromInt <| Time.toDay Time.utc pt

        h =
            String.fromInt <| Time.toHour Time.utc pt

        mi =
            String.fromInt <| Time.toMinute Time.utc pt
    in
    y ++ "-" ++ mo ++ "-" ++ d ++ " " ++ h ++ ":" ++ mi



{-
   day_ : Time
   day_ =
       86400000


   week_ : Time
   week_ =
       604800000


   toDate : String -> Date
   toDate s =
       Date.fromString s |> Result.withDefault (Date.fromTime 0)


   addDays : Date -> Time -> Date
   addDays dx days =
       let
           x =
               toTime dx

           tx =
               x + (days * day_)
       in
       fromTime tx


   addWeeks : Date -> Time -> Date
   addWeeks dx weeks =
       let
           x =
               toTime dx

           tx =
               x + (weeks * week_)
       in
       fromTime tx
-}
{- d0 : earlier date

   d1 : later date

   returns number of days from d0 upto, but not including d1

-}
{-
   diffDays : Date -> Date -> Time
   diffDays d0 d1 =
       let
           t0 =
               toTime d0

           t1 =
               toTime d1
       in
       (t1 - t0) / day_


   dxOf : Date -> Float -> Date
   dxOf dx offset =
       addDays dx offset


   dateRangeOf : Date -> List Float -> ( Date, Date )
   dateRangeOf dx lx =
       let
           offsetHi =
               List.head lx |> Maybe.withDefault 0

           offsetLow =
               M.lastElem lx |> Maybe.withDefault 0
       in
       ( dxOf dx offsetLow, dxOf dx offsetHi )


   monthToInt : Month -> Int
   monthToInt month =
       case month of
           Jan ->
               0

           Feb ->
               1

           Mar ->
               2

           Apr ->
               3

           May ->
               4

           Jun ->
               5

           Jul ->
               6

           Aug ->
               7

           Sep ->
               8

           Oct ->
               9

           Nov ->
               10

           Dec ->
               11

-}
-- 1 + ((t1 - t0) / day_)
