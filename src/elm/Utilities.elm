module Utilities exposing (listGetAt)

-- This code from List.Extra package


listGetAt : Int -> List a -> Maybe a
listGetAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
