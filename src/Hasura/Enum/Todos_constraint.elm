-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Hasura.Enum.Todos_constraint exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| unique or primary key constraints on table "todos"

  - Todos\_pkey - unique or primary key constraint

-}
type Todos_constraint
    = Todos_pkey


list : List Todos_constraint
list =
    [ Todos_pkey ]


decoder : Decoder Todos_constraint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "todos_pkey" ->
                        Decode.succeed Todos_pkey

                    _ ->
                        Decode.fail ("Invalid Todos_constraint type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Todos_constraint -> String
toString enum____ =
    case enum____ of
        Todos_pkey ->
            "todos_pkey"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Todos_constraint
fromString enumString____ =
    case enumString____ of
        "todos_pkey" ->
            Just Todos_pkey

        _ ->
            Nothing
