module Main exposing (Document, GroceryList, Ingredient, Meal, Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { name : String
    , meal : String
    , ingredient : String
    , amount : Float
    , ingredientList : List Ingredient
    , userList : List User
    }


type alias User =
    { name : String
    , userId : Int
    , groceryList : List GroceryList
    , mealList : List Meal
    }


type alias Meal =
    { name : String
    , ingredientList : List Ingredient
    , mealId : Int
    , user : Int
    }


type alias Ingredient =
    { name : String
    , amount : Float
    }


type alias GroceryList =
    { name : String
    , listId : Int
    , meals : List Meal
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = "", meal = "", ingredient = "", amount = 0, ingredientList = [], userList = [] }, Cmd.none )



-- UPDATE


type Msg
    = UserNameInput String
    | SaveUser
    | EditUser User
    | DeleteUser
    | Logout
    | IngredientNameInput String
    | IngredientAmountInput String
    | AddIngredient
    | SaveMeal
    | AddMeal Meal
    | SaveGroceryList GroceryList
    | SelectGroceryList GroceryList
    | EditGroceryList GroceryList
    | DeleteGroceryList GroceryList
    | SelectMeal Meal
    | EditMeal Meal
    | DeleteMeal Meal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserNameInput name ->
            ( { model | name = name }, Cmd.none )

        SaveUser ->
            if String.isEmpty model.name then
                ( model, Cmd.none )

            else
                ( save model, Cmd.none )

        IngredientNameInput ingredient ->
            ( { model | ingredient = ingredient }, Cmd.none )

        IngredientAmountInput amount ->
            ( { model | amount = Maybe.withDefault 0 (String.toFloat amount) }, Cmd.none )

        AddIngredient ->
            Debug.log "Ingredient Added"
                ( { model
                    | ingredientList = Ingredient model.ingredient model.amount :: model.ingredientList
                    , ingredient = ""
                    , amount = 0
                  }
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


save : Model -> Model
save model =
    add model


add : Model -> Model
add model =
    let
        newUser =
            User model.name (List.length model.userList) [] []

        newUserList =
            newUser :: model.userList
    in
    { model
        | userList = newUserList
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Meal to List App"
    , body =
        [ div [] [ h1 [] [ text "Meal to List App" ] ]
        , if validate model then
            userHeader model

          else
            userForm model
        , mealForm model
        ]
    }


userNames : Model -> List String
userNames model =
    List.map (\user -> user.name) model.userList


validate : Model -> Bool
validate model =
    let
        login =
            model.name

        isUser =
            List.filter (\name -> name == login) (userNames model)
    in
    if isUser == [] then
        False

    else
        True


userForm : Model -> Html Msg
userForm model =
    Html.form [ onSubmit SaveUser ]
        [ input
            [ type_ "text"
            , placeholder "Add new user"
            , onInput UserNameInput
            , value model.name
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        ]


userHeader : Model -> Html Msg
userHeader model =
    div []
        [ header [] [ text model.name ]
        , button [ type_ "button", onClick Logout ]
            [ text "Logout" ]
        ]


mealForm : Model -> Html Msg
mealForm model =
    Html.form [ onSubmit AddIngredient ]
        [ input
            [ type_ "text"
            , placeholder "Add new ingredient"
            , onInput IngredientNameInput
            , value model.ingredient
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Amount"
            , onInput IngredientAmountInput
            , value (String.fromFloat model.amount)
            ]
            []
        , button [ type_ "submit" ] [ text "Add" ]
        ]
