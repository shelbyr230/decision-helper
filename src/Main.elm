module Main exposing (main)

import Browser

--use div, h1, text
import Html exposing (Html, div, h1, h2, p, text, input, button)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)

import String


--MAIN
main : Program () Model Msg
main =
    Browser.sandbox 
        { init = init
        , update = update
        , view = view
        }


--MODEL, data changes with updates
type alias Model =
    { decisionTitle : String
    , titleLocked : Bool
    , currentOption : String
    , nextOptionId : Int
    , options : List Option
    , currentCriteria : String
    , nextCriteriaId : Int
    , criteriaList : List Criteria
    }

type alias Option =
    { name : String
    , id : Int
    }

type alias Criteria = 
    { name : String
    , id : Int
    }

init : Model

init = 
    { decisionTitle = "" 
    , titleLocked = False
    , currentOption = ""
    , nextOptionId = 1
    , options = []
    , currentCriteria = ""
    , nextCriteriaId = 1
    , criteriaList = []
    }


--UPDATE, changes data
type Msg
    = UpdateDecisionTitle String
    | SaveDecisionTitle
    | EditDecisionTitle
    | UpdateCurrentOption String
    | AddOption --User clicked add option button
    | DeleteOption Int
    | UpdateCurrentCriteria String
    | AddCriteria --User clicks add criteria button
    | DeleteCriteria Int

update : Msg -> Model -> Model
update msg model = 
    case msg of
        UpdateDecisionTitle newTitle ->
            {model | decisionTitle = newTitle}
        SaveDecisionTitle -> 
            if String.trim model.decisionTitle /= "" then
                {model | titleLocked = True}
            else
                model
        EditDecisionTitle ->
            { model | titleLocked = False }
        UpdateCurrentOption newOption ->
            {model | currentOption = newOption}
        AddOption ->
            if String.trim model.currentOption /= "" then
                let
                    newOption = 
                        { id = model.nextOptionId
                        , name = model.currentOption
                        }
                in
                {model
                    | options = newOption :: model.options
                    , currentOption = ""
                    , nextOptionId = model.nextOptionId + 1
                    }
            else 
                model
        DeleteOption optionId ->
            { model
                | options = 
                    List.filter
                        (\option -> option.id /= optionId)
                        model.options
            }
        UpdateCurrentCriteria newCriteria ->
            {model | currentCriteria = newCriteria}
        AddCriteria ->
            if String.trim model.currentCriteria /= "" then
                let 
                    newCriteria =
                        { id = model.nextCriteriaId
                        , name = model.currentCriteria
                        }
                in
                {model 
                    | criteriaList = newCriteria :: model.criteriaList
                    , currentCriteria = ""
                    , nextCriteriaId = model.nextCriteriaId + 1
                }
            else model
        DeleteCriteria criteriaId ->
            {model
                | criteriaList =
                    List.filter
                        (\criteria -> criteria.id /= criteriaId)
                        model.criteriaList
            }



--VIEW, turns data into HTML
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Decision Helper" ]
        , h2 [] [ text "What decision are you making?"]
        , if model.titleLocked then 
            div []
                [ h2 [] [ text model.decisionTitle ]
                , button [onClick EditDecisionTitle] [text "Edit Decision"]
                ]
        else
            div []
                [ input 
                    [value model.decisionTitle
                    , onInput UpdateDecisionTitle] []
                , button
                    [ onClick SaveDecisionTitle]
                    [text "Save Decision"]
                ]
        , h2 [] [text "Option:"]
        , input 
            [value model.currentOption
            , onInput UpdateCurrentOption] []
        , button [ onClick AddOption ] [ text "Add Option"]
        , h2 [] [text "Options:"]
        , div [] 
            (List.map 
                (\option ->
                    div []
                        [ p [] [ text option.name ]
                        , button 
                            [ onClick (DeleteOption option.id)] 
                            [ text "Delete" ]
                        ]
                )  
                model.options
            ) --transform each item in options list to a paragraph
        , h2 [] [text "Criteria:"]
        , input 
            [value model.currentCriteria
            , onInput UpdateCurrentCriteria] []
        , button [ onClick AddCriteria ] [text "Add Criteria"]
        , div []
            (List.map 
                (\criteria -> 
                    div []
                        [ p [] [ text criteria.name]
                        , button 
                            [ onClick (DeleteCriteria criteria.id)] 
                            [text "Delete"]
                        ]
                ) 
                model.criteriaList
            )
        ]
