module Main exposing (..)

import Array
import Browser
import GraphQLClient exposing (makeGraphQLQuery)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Hasura.Enum.Order_by exposing (Order_by(..))
import Hasura.InputObject
    exposing
        ( Boolean_comparison_exp
        , Todos_bool_exp
        , Todos_order_by
        , buildBoolean_comparison_exp
        , buildTodos_bool_exp
        , buildTodos_order_by
        )
import Hasura.Query exposing (OnlineUsersOptionalArguments, TodosOptionalArguments)



-- INIT

getInitialEvent : String -> Cmd Msg 
getInitialEvent authToken = 
    Cmd.batch
        [ fetchPrivateTodos authToken
        ]

init : ( Model, Cmd Msg )
init =
    ( initalize, Cmd.none )


initalize : Model
initalize =
    { todos = [], is_auth = True }



-- Application Logic


orderByCreatedAt : Order_by -> OptionalArgument (List Todos_order_by)
orderByCreatedAt order =
    Present <| [ buildTodos_order_by (\args -> { args | created_at = OptionalArgument.Present order }) ]


equalToBoolean : Bool -> OptionalArgument Boolean_comparison_exp
equalToBoolean isPublic =
    Present <| buildBoolean_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present isPublic })


whereIsPublic : Bool -> OptionalArgument Todos_bool_exp
whereIsPublic isPublic =
    Present <| buildTodos_bool_exp (\args -> { args | is_public = equalToBoolean isPublic })


todoListOptionalArgument : TodosOptionalArguments -> TodosOptionalArguments
todoListOptionalArgument optionalArgs =
    { optionalArgs | where_ = whereIsPublic False, order_by = orderByCreatedAt Desc }


selectUser : SelectionSet User Hasura.Object.Users
selectUser =
    SelectionSet.map User Users.name


todoListSelection : SelectionSet Todo Hasura.Object.Todo
todoListSelection =
    SelectionSet.map5 Todo
        Todos.id
        Todos.user_id
        Todos.is_completed
        Todos.title
        (Todos.user selctUser)


fetchPrivateTodosQuery : SelectionSet Todos RootQuery
fetchPrivateTodosQuery =
    Query.todos todoListOptionalArgument todoListOptionalArgument


fetchPrivateTodos : String -> Cmd Msg
fetchPrivateTodos authToken =
    makeGraphQLQuery authToken
        fetchPrivateTodosQuery
        (RemoteData.fromResult >> fetchPrivateDataSuccess)


type alias OnlineUser =
    { id : String, user : User }


type alias TodoData =
    RemoteData (Graphql.Http.Error Todos) Todos


type alias PrivateTodo =
    { todos : TodoData
    , visibility : String
    , newTodo : String
    }


type DisplayForm
    = Login
    | Signup





initializePrivateTodo : PrivateTodo
initializePrivateTodo =
    { todos = RemoteData.Loading
    , visibility = "All"
    , newTodo = ""
    , mutateTodo = GraphQLResponse RemoteData.NotAsked
    }
type alias AuthData =
    { username : String
    , email : String
    , password : String
    }
type alias PrivateTodo = 
    { todos : List Int
    }

type alias Model =
    { privateData : PrivateTodo
    , publicTodoInsert : String
    , publicTodoInfo : PublicTodoData
    , online_users : OnlineUsers
    , authData : AuthData
    , authForm : AuthForm
    }

type Msg
    = EnterEmail String
    | EnterPassword String
    | EnterUsername String
    | MakeLoginRequest
    | MakeSignupRequest
    | ToggleAuthForm DisplayForm
    | GotLoginResponse LoginResponseParser
    | GotSignupResponse SignupResponseParser
    | ClearAuthToken
    | FetchPrivateDataSuccess TodoData
