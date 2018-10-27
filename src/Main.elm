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
    , selectedMeal : List Meal
    , selectedGrocery : List Grocery
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
      , selectedMeal = []
      , selectedGrocery = []
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
    | SelectMeal String
    | SelectGrocery String 
    | CancelSelectedMeal
    | CancelSelectedGrocery 
    | EditMealItem Item 
    | EditGroceryItem Item
    | DeleteMealItem Item  
    | DeleteGroceryItem Item
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
            ( createItem model, Cmd.none )

        AddMeal ->
            ( addItems model, Cmd.none )

        SaveMeal ->
            ( createMeal model, Cmd.none )

        SaveGrocery ->
            ( createGrocery model, Cmd.none )

        SelectMeal meal ->
            ( { model
                | selectedMeal = 
                    findSelectedMeal model meal }, Cmd.none )

        SelectGrocery grocery ->
            ( { model
                | selectedGrocery =
                    findSelectedGrocery model grocery }, Cmd.none )

        CancelSelectedMeal -> 
            ( { model 
                | selectedMeal = [] }, Cmd.none )

        CancelSelectedGrocery -> 
            ( { model 
                | selectedGrocery = [] }, Cmd.none )

        DeleteMealItem item -> 
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


--find selected grocery list and display

findSelectedGrocery : Model -> String -> List Grocery  
findSelectedGrocery model name =
    List.filter (\grocery -> name == grocery.name ) model.groceryListAll 

-- find selected meal and display 

findSelectedMeal : Model -> String -> List Meal  
findSelectedMeal model name =
    List.filter (\meal -> name == meal.name ) model.mealList 

-- format input for comparison purposes


formatString : String -> String
formatString input =
    input
        |> String.toLower
        |> String.trim



--save a new grocery list, used by SaveGrocery


createGrocery : Model -> Model
createGrocery model =
    let
        newGrocery =
            Grocery (formatString model.grocery) model.groceryList

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

        newMealList =
            meal
                ++ model.groceryList
                |> List.sortBy .name
                |> convertToDictAndSumAmount
                |> Dict.values
    in
    { model | groceryList = newMealList }


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
            Meal (formatString model.meal) model.itemList

        newMealList =
            newMeal :: model.mealList
    in
    { model
        | mealList = newMealList
        , meal = ""
        , itemList = []
    }



-- create a new item to the item list


createItem : Model -> Model
createItem model =
    { model
        | itemList = Item (formatString model.item) model.amount model.unit :: model.itemList
        , item = ""
        , amount = 0
        , unit = ""
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
        [ titleDiv
        , lineBreak
        , mealFormHeader
        , mealForm model
        , itemSection model
        , lineBreak
        , groceryFormHeader
        , groceryForm model
        , grocerySection model
        , lineBreak
        , mealListSelection model
        , if (model.selectedMeal /= []) then selectedMeal model else div [ hidden True ] []
        , lineBreak
        , groceryListSelection model
        , if (model.selectedGrocery /= []) then selectedGrocery model else div [ hidden True ] []
        ]
    }


titleDiv : Html Msg
titleDiv =
    div [ class "page-header" ] [ h1 [ id "title" ] [ text "Meal to List App" ] ]


lineBreak : Html Msg
lineBreak =
    div [ class "header-break" ] []


-- grocery list selection component

selectedGroceryName : Model -> Html Msg
selectedGroceryName model =
    div [] 
        [ h2 [] 
            (List.map(\grocery -> text grocery.name) model.selectedGrocery)
            , h2 [] [ i [ class "fas fa-ban", onClick CancelSelectedGrocery ] []]
        ]

selectedGroceryItemMod : Item -> Html Msg
selectedGroceryItemMod item =
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
        , div   
            [] 
            [ p [] 
                [ i [ class "fas fa-edit", onClick (EditGroceryItem item) ] []
                ]
            ]
        , div   
            [] 
            [ p [] 
                [ i [ class "fas fa-trash", onClick (DeleteGroceryItem item) ] []
                ]
            ]
        ]

selectedGroceryItems : Model -> Html Msg
selectedGroceryItems model =
    model.selectedGrocery
        |> List.foldl (\grocery list-> grocery.items ++ list ) []
        |> List.map selectedGroceryItemMod
        |> ul []

--, i [ class "fas fa-edit"] []

selectedGroceryBody : Model -> Html Msg
selectedGroceryBody model =
    div [] 
        [ itemListHeader
        , selectedGroceryItems model
    ]


selectedGrocery : Model -> Html Msg
selectedGrocery model =
    div []
        [ selectedGroceryName model 
        , selectedGroceryBody model
        ]


groceryListSelectionHeader : Html Msg
groceryListSelectionHeader =
    h2 [ class "component-header" ] [ text "Grocery Lists" ]


groceryNameList : Model -> List (Html Msg)
groceryNameList model =
    List.map (\grocery -> li [ onClick (SelectGrocery grocery.name) ] [ text grocery.name ]) model.groceryListAll


groceryListSelection : Model -> Html Msg
groceryListSelection model =
    div []
        [ groceryListSelectionHeader
        , ul [] (groceryNameList model)
        ]



-- meal list selection component


selectedMealName : Model -> Html Msg
selectedMealName model =
    div [] 
        [ h2 [] 
            (List.map(\meal -> text meal.name) model.selectedMeal)
            , h2 [] [ i [ class "fas fa-ban", onClick CancelSelectedMeal ] []]
        ]

selectedMealItemMod : Item -> Html Msg
selectedMealItemMod item =
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
        , div   
            [] 
            [ p [] 
                [ i [ class "fas fa-edit", onClick (EditMealItem item) ] []
                ]
            ]
        , div   
            [] 
            [ p [] 
                [ i [ class "fas fa-trash", onClick (DeleteMealItem item) ] []
                ]
            ]
        ]

selectedMealItems : Model -> Html Msg
selectedMealItems model =
    model.selectedMeal
        |> List.foldl (\meal list-> meal.itemList ++ list ) []
        |> List.map selectedMealItemMod
        |> ul []

selectedMealBody : Model -> Html Msg
selectedMealBody model =
    div [] 
        [ itemListHeader
        , selectedMealItems model
    ]


selectedMeal : Model -> Html Msg
selectedMeal model =
    div []
        [ selectedMealName model 
        , selectedMealBody model
        ]


mealListSelectionHeader : Html Msg
mealListSelectionHeader =
    h2 [ class "component-header" ] [ text "Meals" ]


mealNameList : Model -> List (Html Msg)
mealNameList model =
    List.map (\meal -> li [ onClick (SelectMeal meal.name) ] [ text meal.name ]) model.mealList


mealListSelection : Model -> Html Msg
mealListSelection model =
    div []
        [ mealListSelectionHeader
        , ul [] (mealNameList model)
        ]



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
                , required True
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
                [ select
                    [ onInput InputMeal
                    , required True
                    , class "form-input"
                    , value model.meal
                    ]
                    (option [] [ text "choose a meal" ] :: mealSelect model)
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
                , required True
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
                    , required True
                    ]
                    []
                , input
                    [ class "form-input"
                    , type_ "text"
                    , required True
                    , placeholder "Amount"
                    , onInput InputAmount
                    , value
                        (model.amount
                            |> String.fromFloat
                            |> formatString
                        )
                    ]
                    []
                , select
                    [ class "form-input"
                    , onInput InputUnit
                    , value model.unit
                    , required True
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
