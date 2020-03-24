module Common.Util exposing
    ( asHttpBody
    , flip
    , httpErr2str
    , lastElem
    , listAsHttpBody
    , toDecimal
    , unpackMaybe
    )

import Http
import Json.Encode as JE


flip : (a -> b -> c) -> b -> a -> c
flip fn x y =
    fn y x


toDecimal : Float -> Float -> Float
toDecimal value roundFactor =
    let
        valx =
            toFloat <| round <| value * roundFactor
    in
    valx / roundFactor


asHttpBody : List ( String, JE.Value ) -> Http.Body
asHttpBody lx =
    let
        x =
            JE.object lx
    in
    Http.stringBody "application/json" (JE.encode 0 x)


listAsHttpBody : List (List ( String, JE.Value )) -> Http.Body
listAsHttpBody lx =
    let
        xx =
            --JE.list (List.map (\x -> JE.object x) lx)
            JE.list JE.object lx
    in
    Http.stringBody "application/json" (JE.encode 0 xx)


httpErr2str : Http.Error -> String
httpErr2str err =
    case err of
        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadUrl s ->
            "BadUrl: " ++ s

        Http.BadStatus r ->
            "Server Error: " ++ r.body

        Http.BadPayload s r ->
            "BadPayload: " ++ s ++ ", " ++ r.body



{-
   findInList : List (Oidable a) -> Int -> Maybe (Oidable a)
   findInList lx oid =
       List.head <| List.filter (\x -> x.oid == oid) lx


   replaceWith : Oidable a -> Oidable a -> Oidable a
   replaceWith newEl el =
       if el.oid == newEl.oid then
           newEl

       else
           el
-}


unpackMaybe : Maybe a -> (a -> b) -> b -> b
unpackMaybe obj fn default =
    Maybe.withDefault default <| Maybe.map fn obj


lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing
