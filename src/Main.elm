module Main exposing (main)

import Browser
import Html exposing (..)
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
import Hasura.Object.Todos exposing (is_completed)
import Constants exposing (failMsg, signup_url, login_url)
import Http 
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Time 
import Html.Keyed as Keyed
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Html.Attributes exposing (..)

-- MAIN 
main : Program () Model Msg 
main = Browser.element 
    { view = view
    , init = \_ -> init 
    , update = update
    , subscriptions = subscriptions 
    }

-- SUBSCRIPTIONS 
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <| 
    [ GotStoredToken GotStoredToken] 
    ++ (case String.length model.authData.authToken of 
            0 -> []
            _ -> [ gotRecentPublicTodoItem RecentPublicTodoReceived
                 , gotOnlineUsers GotOnlineUsers
                 , Time.every 30000 Tick
                 ]
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
    { privateDate  = initializePrivateTodo
    , online_users = RemoteData.NotAsked
    , publicTodoInsert  = ""
    , publicTodoInfo = PublicTodoData [] 0 0 0 True 
    , authData = AuthData "" "" "" "" 
    , authForm = AuthForm Login False False False ""
    , publicTodoInsertStatus = NotYetInitiated 
    , publicTodoLoadingStatus = False 
    } 


-- Types 
type alias Todo  = 
    { id:Int
    , user_id: String
    , is_completed: Bool
    , title: String
    , user: User
    }
type alias Todos = List Todo
-- Application Logic

type alias User = { name : String } 


type alias OnlineUser =
    { id : String, user : User }
type alias OnlineUsers = List OnlineUser 

type alias OnlineUsersData = RemoteData Decode.Error OnlineUsers

type alias PublicDataFetched = RemoteData (Graphql.Http.Error Todos) Todos 
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

type alias AuthData =
    { username : String
    , email : String
    , password : String
    }

type alias PublicTodoData = 
    { todos : Todos 
    , oldestTodoId : Int
    , newTodoCound : Int 
    , currentLastTodoId : Int 
    , oldTodosAvailible : Int 
    }
type alias AuthForm = 
    { displayForm : DisplayForm
    , isRequestInProgress : Bool 
    , isSignupSuccess : Bool 
    , requestError : String
    }
type alias Model =
    { privateData : PrivateTodo
    , publicTodoInsert : String
    , publicTodoInfo : PublicTodoData
    , online_users : List OnlineUser 
    , authData : AuthData
    , authForm : AuthForm
    , publicTodoInsertStatus : Operation 
    , publicTodoLoadingStatus : Bool 
    }

type Operation 
    = NotYetInitiated 
    | OnGoing 
    | OperationFailed String 
    
type alias SignupResponseParser =
    RemoteData Http.Error SignupResponseData


type alias SignupResponseData =
    { id : String }


type alias LoginResponseData =
    { token : String }



type alias LoginResponseParser =
    RemoteData Http.Error LoginResponseData




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


todoListSelection : SelectionSet Todo Hasura.Object.Todos
todoListSelection =
    SelectionSet.map5 Todo
        Todos.id
        Todos.user_id
        Todos.is_completed
        Todos.title
        (Todos.user selectUser)


fetchPrivateTodosQuery : SelectionSet Todos RootQuery
fetchPrivateTodosQuery =
    Query.todos todoListOptionalArgument todoListOptionalArgument


fetchPrivateTodos : String -> Cmd Msg
fetchPrivateTodos authToken =
    makeGraphQLQuery authToken
        fetchPrivateTodosQuery
        (RemoteData.fromResult >> fetchPrivateDataSuccess)





initializePrivateTodo : PrivateTodo
initializePrivateTodo =
    { todos = RemoteData.Loading
    , visibility = "All"
    , newTodo = ""
    , mutateTodo = GraphQLResponse RemoteData.NotAsked
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
    | Tick Time.Posix
    | MarkCompleted Int Bool 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        MarkCompleted id completed -> 
            let 
                updateObj = updateTodoStatus id (not completed)
            in
                (model, updateTodoList updateObj model.authData.authToken)
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

updateTodoStatus : Int -> Bool -> SelectionSet (Maybe MutationResponse) RootMutation
updateTodoStatus todoId status = 
    Mutation.update_todos (setTodoListUpdateArgs status) (setTodoListUpdateWhere todoId) mutationResponseSelection

updateAuthData : (AuthData -> AuthData) -> Model -> Cmd Msg -> (Model, Cmd Msg)
updateAuthData transform model cmd = ({model | authData = transform model.authData}, cmd)

updatePrivateData : (PrivateTodo -> PrivateTodo) -> Model -> Cmd Msg -> (Model, Cmd Msg)
updatePrivateData transform model cmd = ({model | privateData = transform model.privateData}, cmd) 


-- VIEW 
view : Model -> Html Msg 
view model = div [] [] 
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

todoListWrapper : String -> Todos -> Html Msg 
todoListWrapper visibility todos = 
    div []  [ Keyed.ul [] <| 
                List.map 
                    viewKeyedListItem 
                    (List.filter (filterTodos visibility) todos)
    ]
viewKeyedListItem  : Todo -> (String, Html Msg) 
viewKeyedListItem todo = 
    (String.fromInt todo.id, viewListItem todo)

viewListItem : Todo -> Html Msg 
viewListItem todo = 
    li 
    [] 
    [ input 
            [ checked todo.is_completed
            , type_ "checkbox"
            , id (String.fromInt todo.id)
            , onClick (MarkCompleted todo.id todo.is_completed)
            ]
            []
    , label [for (String.fromInt todo.id)] []
   ]
        
filterTodos : String -> Todo -> Bool 
filterTodos visibility todo = 
    case visibility of 
        "Completed" -> 
            todo.is_completed
        "Active" -> 
            not todo.is_completed
        _ -> 
            True 