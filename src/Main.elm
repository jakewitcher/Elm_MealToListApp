module Main exposing (Document, Grocery, Item, Meal, Model, Msg(..), createMeal, init, itemList, itemListHeader, itemMod, itemSection, main, mealForm, subscriptions, update, view)

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
    , item : String
    , amount : Float
    , unit : String
    , itemList : List Item
    , mealList : List Meal
    , groceryList : List Item
    , groceryListAll : List Grocery
    }


type alias Item =
    { name : String
    , amount : Float
    , unit : String
    }


type alias Meal =
    { name : String
    , itemList : List Item
    }


type alias Grocery =
    { name : String
    , items : List Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { meal = "", item = "", amount = 0, unit = "", itemList = [], mealList = [], groceryList = [], groceryListAll = [] }, Cmd.none )



-- UPDATE


type Msg
    = ItemInput String
    | AmountInput String
    | UnitInput String
    | MealNameInput String
    | Additem
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
        ItemInput item ->
            ( { model | item = item }, Cmd.none )

        AmountInput amount ->
            ( { model | amount = Maybe.withDefault 0 (String.toFloat amount) }, Cmd.none )

        UnitInput unit ->
            ( { model | unit = unit }, Cmd.none )

        Additem ->
            ( { model
                | itemList = Item model.item model.amount model.unit :: model.itemList
                , item = ""
                , amount = 0
                , unit = ""
              }
            , Cmd.none
            )

        MealNameInput meal ->
            ( { model | meal = meal }, Cmd.none )

        SaveMeal ->
            Debug.log "Save meal"
                ( createMeal model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- add list of items from a meal with the required amount to a grocery list
-- add a new meal to the meal list, used by SaveMeal


createMeal : Model -> Model
createMeal model =
    let
        newMeal =
            Meal model.meal model.itemList

        newMealList =
            newMeal :: model.mealList
    in
    { model
        | mealList = newMealList
        , meal = ""
        , itemList = []
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
        , itemSection model
        ]
    }



-- component displaying the list of items and the amounts being added to a new meal


itemSection : Model -> Html Msg
itemSection model =
    div []
        [ itemListHeader
        , itemList model
        ]


itemListHeader : Html Msg
itemListHeader =
    header []
        [ div
            []
            [ h4 [] [ text "Item" ]
            , h4 [] [ text "Amount" ]
            , h4 [] [ text "Unit" ]
            ]
        ]


itemList : Model -> Html Msg
itemList model =
    model.itemList
        |> List.map itemMod
        |> ul []


itemMod : Item -> Html Msg
itemMod item =
    li []
        [ div
            []
            [ p [] [ text item.name ]
            ]
        , div
            []
            [ p [] [ text (String.fromFloat item.amount) ]
            ]
        , div
            []
            [ p [] [ text item.unit ]
            ]
        ]



-- Form for making a new meal. item name and amount is added to the list one at a time, then saved as a meal


mealForm : Model -> Html Msg
mealForm model =
    div []
        [ div []
            [ input
                [ type_ "text"
                , placeholder "Meal name"
                , onInput MealNameInput
                , value model.meal
                ]
                []
            , button [ type_ "button", onClick SaveMeal ] [ text "Save meal" ]
            ]
        , div []
            [ Html.form [ onSubmit Additem ]
                [ input
                    [ type_ "text"
                    , placeholder "Add new item"
                    , onInput ItemInput
                    , value model.item
                    , selected True
                    ]
                    []
                , input
                    [ type_ "text"
                    , placeholder "Amount"
                    , onInput AmountInput
                    , value (String.fromFloat model.amount)
                    ]
                    []
                , select
                    [ onInput UnitInput
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
            ]
        ]
