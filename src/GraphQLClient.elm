module GraphQLClient exposing (makeGraphQLQuery, makeGraphQLMutation)

import Graphql.Http
import Graphql.Operation exposing (RootQuery, RootMutation)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)


graphql_url : String
graphql_url =
    "https://hasura.io/learn/graphql"


getAuthHeader :
    String
    -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)

makeGraphQLQuery :
    String
    -> SelectionSet decodesTo RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
makeGraphQLQuery authToken query decodesTo =
    query
        |> Graphql.Http.queryRequest graphql_url
        |> getAuthHeader authToken
        |> Graphql.Http.send decodesTo

makeGraphQLMutation : 
    String
    -> SelectionSet decodesTo RootMutation 
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg

makeGraphQLMutation authToken query decodesTo = 
    query 
        |> Graphql.Http.mutationRequest graphql_url
        |> getAuthHeader authToken 
        |> Graphql.Http.send decodesTo 