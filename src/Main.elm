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
    , grocery : String
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
    ( { meal = ""
      , item = ""
      , amount = 0
      , unit = ""
      , grocery = ""
      , itemList = []
      , mealList = []
      , groceryList = []
      , groceryListAll = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputItem String
    | InputAmount String
    | InputUnit String
    | InputMeal String
    | InputGrocery String
    | AddItem
    | AddMeal
    | SaveMeal
    | SaveGrocery
    | SelectMeal
    | SelectGrocery
    | EditMeal
    | EditGrocery
    | DeleteMeal
    | DeleteGrocery


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputItem item ->
            ( { model | item = item }, Cmd.none )

        InputAmount amount ->
            ( { model | amount = Maybe.withDefault 0 (String.toFloat amount) }, Cmd.none )

        InputUnit unit ->
            ( { model | unit = unit }, Cmd.none )

        InputMeal meal ->
            ( { model | meal = meal }, Cmd.none )

        InputGrocery grocery ->
            ( { model | grocery = grocery }, Cmd.none )

        AddItem ->
            ( { model
                | itemList = Item model.item model.amount model.unit :: model.itemList
                , item = ""
                , amount = 0
                , unit = ""
              }
            , Cmd.none
            )

        AddMeal ->
            ( addItems model, Cmd.none )

        SaveMeal ->
            Debug.log "Save meal"
                ( createMeal model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- add list of items from a meal to a grocery list, increasing the amount if the item already exists in the grocery list


findMeal : Model -> List Meal
findMeal model =
    List.filter (\meal -> meal.name == model.meal) model.mealList


addItems : Model -> Model
addItems model =
    let
        meal =
            findMeal model
                |> List.map (\m -> m.itemList)
                |> List.foldl (++) []
    in
    { model
        | groceryList = meal ++ model.groceryList
    }



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
        [ div [ class "title" ] [ h1 [] [ text "Meal to List App" ] ]
        , mealForm model
        , itemSection model
        , groceryForm model
        , grocerySection model
        ]
    }



-- component displaying the list of items and the amounts being added to a new grocery list


grocerySection : Model -> Html Msg
grocerySection model =
    div []
        [ itemListHeader
        , groceryList model
        ]


groceryList : Model -> Html Msg
groceryList model =
    model.groceryList
        |> List.map itemMod
        |> ul []



-- form for selecting the meals you would like to combine into a single grocery list


mealSelect : Model -> List (Html Msg)
mealSelect model =
    List.map (\meal -> option [] [ text (.name meal) ]) model.mealList


groceryForm : Model -> Html Msg
groceryForm model =
    div [ class "component" ]
        [ input
            [ type_ "text"
            , placeholder "Grocery list name"
            , onInput InputGrocery
            , value model.grocery
            ]
            []
        , button [ type_ "button", onClick SaveGrocery ] [ text "Save grocery list" ]
        , Html.form [ onSubmit AddMeal ]
            [ select [ onInput InputMeal, value model.meal ] (mealSelect model)
            , button [ type_ "submit" ] [ text "Add to grocery list" ]
            ]
        ]



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
            [ class "header" ]
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
    li [ class "header" ]
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
        [ div [ class "component" ]
            [ input
                [ type_ "text"
                , placeholder "Meal name"
                , onInput InputMeal
                , value model.meal
                ]
                []
            , button [ type_ "button", onClick SaveMeal ] [ text "Save meal" ]
            ]
        , div [ class "component" ]
            [ Html.form [ onSubmit AddItem ]
                [ input
                    [ type_ "text"
                    , placeholder "Add new item"
                    , onInput InputItem
                    , value model.item
                    , selected True
                    ]
                    []
                , input
                    [ type_ "text"
                    , placeholder "Amount"
                    , onInput InputAmount
                    , value (String.fromFloat model.amount)
                    ]
                    []
                , select
                    [ onInput InputUnit
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
