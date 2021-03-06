module Main exposing (main)

import Array
import Browser
import GraphQLClient exposing (makeGraphQLQuery)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Hasura.Object 
import Hasura.Object.Todos as Todos 
import Hasura.Object.Users as Users
import Hasura.Enum.Order_by exposing (Order_by(..))
import Hasura.InputObject
    exposing
        ( Boolean_comparison_exp
        , Todos_bool_exp
        , Todos_order_by
        , buildBoolean_comparison_exp
        , buildTodos_bool_exp
        , buildTodos_order_by
        , Int_comparison_exp
        , Todos_set_input
        , buildInt_comparison_exp
        , buildTodos_set_input
        )
import RemoteData exposing (RemoteData)
import Hasura.Query 
    exposing 
        (OnlineUsersOptionalArguments, TodosOptionalArguments)

import Hasura.Mutation
    exposing 
        ( InsertTodosRequiredArguments
        , insert_todos
        , UpdateTodosOptionalArguments
        , UpdateTodosRequiredArguments
        )


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


type alias User = {}
type alias OnlineUser =
    { id : String, user : User }

type alias Todos = {}
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
    | GotStoredToken String 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        EnterEmail em -> 
            updateAuthData (\authData -> { authData | email = em}) model Cmd.none
        EnterPassword  pswd-> 
            updateAuthData (\authData -> { authData | password = pswd}) model Cmd.none
        EnterUsername name -> 
            updateAuthData (\authData -> { authData | username = name}) model Cmd.none
        MakeLoginRequest -> 
            (model, Cmd.none)
        MakeSignupRequest -> 
            (model, Cmd.none)
        ToggleAuthForm _ -> 
            (model, Cmd.none)
        GotStoredToken token -> 
            updateAuthData 
                (\authData -> {authData | authToken = token })
                model 
                (if token == "" then Cmd.none else getInitialEvent token)
        GotLoginResponse data -> 
            case data of 
                RemoteData.Success d -> 
                    updateAuthAndFormData 
                        (\authForm -> {authForm | isRequestInProgress = False, isSignupSuccess = False})
                        (\authData -> {authData | authTOken = d.token})
                        model 
                        (Cmd.batch [storeToken d.token, getInitialEvent d.token])
                Remote.Failure err -> 
                    updateAuthFormData 
                        (\authForm -> {authForm | isRequestInProgress = False, requestError = "Unable to authenticate"})
                        model 
                        Cmd.none
                _ -> 
                    (model, Cmd.none)

        GotSignupResponse _ -> 
            (model, Cmd.none)
        ClearAuthToken -> 
            (model, Cmd.none)
        FetchPrivateDataSuccess response -> 
            updatePrivateData (\privateDate -> {privateData | todos = response }) model Cmd.none

updateAuthData : (AuthData -> AuthData) -> Model -> Cmd Msg -> (Model, Cmd Msg)
updateAuthData transform model cmd = ({model | authData = tranform model.authData}, cmd)

updatePrivateData : (PrivateTodo -> PrivateTodo) -> Model -> Cmd Msg -> (Model, Cmd Msg)
updatePrivateData transform model cmd = ({model | privateData = transform model.privateData}, cmd) 

renderTodos : PrivateTodo -> Html Msg 
renderTodos privateData = 
    div [] <| 
        case privateData.todos of 
            RemoteData.NotAsked -> 
                [text "not asked"]
            RemoteData.Success todos -> 
                [ todoListWrapper privateData.visibility todos ]
            RemoteData.Loading -> 
                [ span [] [ text "Loading todo..."]]
            RemoteData.Failure err -> 
                [text "Error Loading Todos"]
