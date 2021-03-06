-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Hasura.Object.Users exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Hasura.Enum.Todos_select_column
import Hasura.InputObject
import Hasura.Interface
import Hasura.Object
import Hasura.Scalar
import Hasura.ScalarCodecs
import Hasura.Union
import Json.Decode as Decode


id : SelectionSet String Hasura.Object.Users
id =
    Object.selectionForField "String" "id" [] Decode.string


name : SelectionSet String Hasura.Object.Users
name =
    Object.selectionForField "String" "name" [] Decode.string


type alias TodosOptionalArguments =
    { distinct_on : OptionalArgument (List Hasura.Enum.Todos_select_column.Todos_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Hasura.InputObject.Todos_order_by)
    , where_ : OptionalArgument Hasura.InputObject.Todos_bool_exp
    }


{-| An array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
todos :
    (TodosOptionalArguments -> TodosOptionalArguments)
    -> SelectionSet decodesTo Hasura.Object.Todos
    -> SelectionSet (List decodesTo) Hasura.Object.Users
todos fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Hasura.Enum.Todos_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Hasura.InputObject.encodeTodos_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Hasura.InputObject.encodeTodos_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "todos" optionalArgs____ object____ (identity >> Decode.list)
