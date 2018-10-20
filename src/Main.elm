module Main exposing (Document, Grocery, Ingredient, Meal, Model, Msg(..), init, main, subscriptions, update, view)

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
    { meal : String
    , ingredient : String
    , amount : Float
    , unit : String
    , ingredientList : List Ingredient
    , mealList : List Meal
    , groceryList : List Grocery
    }


type alias Ingredient =
    { name : String
    , amount : Float
    , unit : String
    }


type alias Meal =
    { name : String
    , ingredientList : List Ingredient
    }


type alias Grocery =
    { name : String
    , meals : List Meal
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { meal = "", ingredient = "", amount = 0, unit = "", ingredientList = [], mealList = [], groceryList = [] }, Cmd.none )



-- UPDATE


type Msg
    = IngredientNameInput String
    | IngredientAmountInput String
    | IngredientunitInput String
    | MealNameInput String
    | AddIngredient
    | SaveMeal
    | AddMeal
    | SaveGroceryList
    | SelectGroceryList
    | EditGroceryList
    | DeleteGroceryList
    | SelectMeal
    | EditMeal
    | DeleteMeal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IngredientNameInput ingredient ->
            ( { model | ingredient = ingredient }, Cmd.none )

        IngredientAmountInput amount ->
            ( { model | amount = Maybe.withDefault 0 (String.toFloat amount) }, Cmd.none )

        IngredientunitInput unit ->
            ( { model | unit = unit }, Cmd.none )

        AddIngredient ->
            ( { model
                | ingredientList = Ingredient model.ingredient model.amount model.unit :: model.ingredientList
                , ingredient = ""
                , amount = 0
                , unit = ""
              }
            , Cmd.none
            )

        MealNameInput meal ->
            ( { model | meal = meal }, Cmd.none )

        SaveMeal ->
            Debug.log "Save meal"
                ( addMeal model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- add a new meal to the meal list, used by SaveMeal


addMeal : Model -> Model
addMeal model =
    let
        newMeal =
            Meal model.meal model.ingredientList

        newMealList =
            newMeal :: model.mealList
    in
    { model
        | mealList = newMealList
        , meal = ""
        , ingredientList = []
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
        , mealForm model
        , ingredientSection model
        ]
    }



-- component displaying the list of ingredients and the amounts being added to a new meal


ingredientSection : Model -> Html Msg
ingredientSection model =
    div []
        [ ingredientListHeader
        , ingredientList model
        ]


ingredientListHeader : Html Msg
ingredientListHeader =
    header []
        [ div
            []
            [ h4 [] [ text "Ingredient" ]
            , h4 [] [ text "Amount" ]
            , h4 [] [ text "Unit" ]
            ]
        ]


ingredientList : Model -> Html Msg
ingredientList model =
    model.ingredientList
        |> List.map ingredientMod
        |> ul []


ingredientMod : Ingredient -> Html Msg
ingredientMod ingredient =
    li []
        [ div
            []
            [ p [] [ text ingredient.name ]
            ]
        , div
            []
            [ p [] [ text (String.fromFloat ingredient.amount) ]
            ]
        , div
            []
            [ p [] [ text ingredient.unit ]
            ]
        ]



-- FORM FOR MAKING A NEW MEAL. INGREDIENT NAME AND AMOUNT IS ADDED TO THE LIST ONE AT A TIME, THEN SAVED AS A MEAL


mealForm : Model -> Html Msg
mealForm model =
    div []
        [ Html.form [ onSubmit AddIngredient ]
            [ input
                [ type_ "text"
                , placeholder "Add new ingredient"
                , onInput IngredientNameInput
                , value model.ingredient
                , selected True
                ]
                []
            , input
                [ type_ "text"
                , placeholder "Amount"
                , onInput IngredientAmountInput
                , value (String.fromFloat model.amount)
                ]
                []
            , select
                [ onInput IngredientunitInput
                , value model.unit
                ]
                [ option [] [ text "lbs" ]
                , option [] [ text "pkg" ]
                , option [] [ text "qrt" ]
                , option [] [ text "gal" ]
                , option [] [ text "pnt" ]
                , option [] [ text "qty" ]
                ]
            , button [ type_ "submit" ] [ text "Add" ]
            ]
        , div []
            [ input
                [ type_ "text"
                , placeholder "Meal name"
                , onInput MealNameInput
                , value model.meal
                ]
                []
            , button [ type_ "button", onClick SaveMeal ] [ text "Save meal" ]
            ]
        ]
