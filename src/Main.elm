module Main exposing (Document, Grocery, Item, Meal, Model, Msg(..), createMeal, init, itemList, itemListHeader, itemMod, itemSection, main, mealForm, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
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
            Debug.log "Add an item to a meal list"
                ( { model
                    | itemList = Item model.item model.amount model.unit :: model.itemList
                    , item = ""
                    , amount = 0
                    , unit = ""
                }
                , Cmd.none
                )

        AddMeal ->
            Debug.log "Add a meal to a grocery list"
                ( addItems model, Cmd.none )

        SaveMeal ->
            Debug.log "New meal list"
                ( createMeal model, Cmd.none )
        SaveGrocery -> 
            Debug.log "New grocery list" 
                ( createGrocery model, Cmd.none )
        _ ->
            ( model, Cmd.none )


--save a new grocery list, used by SaveGrocery

createGrocery : Model -> Model
createGrocery model =
    let
        newGrocery =
            Grocery model.grocery model.groceryList

        newGroceryList =
            newGrocery :: model.groceryListAll
    in
    { model
        | groceryListAll = newGroceryList
        , grocery = ""
        , groceryList = []
    }


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
                |> convertToDictAndSumAmount
                |> Dict.values
    in
    { model
        | groceryList = List.sortBy .name (meal ++ model.groceryList)
    }


convertToDictAndSumAmount : List Item -> Dict String Item
convertToDictAndSumAmount meals =
    let
        newDict : Dict String Item
        newDict =
            Dict.empty
    in
    List.foldl
        (\item dict ->
            let
                itemKey =
                    .name item
            in
            if Dict.member itemKey dict then
                Dict.update itemKey (Maybe.map (\record -> { record | amount = record.amount + item.amount })) dict

            else
                Dict.insert itemKey item dict
        )
        newDict
        meals



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
        [ div [ class "page-header" ] [ h1 [ id "title" ] [ text "Meal to List App" ] ]
        , div [ class "header-break" ] []
        , mealFormHeader
        , mealForm model
        , itemSection model
        , div [ class "header-break" ] []
        , groceryFormHeader
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


groceryFormHeader : Html Msg
groceryFormHeader =
    div [ class "component-header" ]
        [ h2 []
            [ text "Create a new grocery list" ]
        ]


mealSelect : Model -> List (Html Msg)
mealSelect model =
    List.map (\meal -> option [] [ text (.name meal) ]) model.mealList


groceryForm : Model -> Html Msg
groceryForm model =
    div [ class "form-component" ]
        [ div [ class "form-name-box" ]
            [ input
                [ type_ "text"
                , class "form-input"
                , placeholder "Grocery list name"
                , onInput InputGrocery
                , value model.grocery
                ]
                []
            , button [ type_ "button", class "form-button", onClick SaveGrocery ] [ text "Save grocery list" ]
            ]
        , div [ class "form-item-box" ]
            [ Html.form [ onSubmit AddMeal ]
                [ select [ onInput InputMeal
                    , class "form-input"
                    , value model.meal ] 
                        ((option [] [ text "choose a meal" ]) :: (mealSelect model))
                , button [ type_ "submit", class "form-button" ] [ text "Add to grocery list" ]
                ]
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
            [ class "item-list-header" ]
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
    li [ class "item-list" ]
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


mealFormHeader : Html Msg
mealFormHeader =
    div [ class "component-header" ]
        [ h2 []
            [ text "Create a new meal" ]
        ]


mealForm : Model -> Html Msg
mealForm model =
    div [ class "form-component" ]
        [ div [ class "form-name-box" ]
            [ input
                [ class "form-input"
                , type_ "text"
                , placeholder "Meal name"
                , onInput InputMeal
                , value model.meal
                ]
                []
            , button [ type_ "button", class "form-button", onClick SaveMeal ] [ text "Save meal" ]
            ]
        , div [ class "form-item-box" ]
            [ Html.form [ onSubmit AddItem ]
                [ input
                    [ class "form-input"
                    , type_ "text"
                    , placeholder "Add new item"
                    , onInput InputItem
                    , value model.item
                    , selected True
                    ]
                    []
                , input
                    [ class "form-input"
                    , type_ "text"
                    , placeholder "Amount"
                    , onInput InputAmount
                    , value (String.fromFloat model.amount)
                    ]
                    []
                , select
                    [ class "form-input"
                    , onInput InputUnit
                    , value model.unit
                    ]
                    [ option [] [ text "pound(s)" ]
                    , option [] [ text "ounce" ]
                    , option [] [ text "gallon" ]
                    , option [] [ text "quart" ]
                    , option [] [ text "pint" ]
                    , option [] [ text "liter" ]
                    , option [] [ text "item" ]
                    , option [] [ text "package" ]
                    ]
                , button [ type_ "submit", class "form-button" ] [ text "Add" ]
                ]
            ]
        ]
