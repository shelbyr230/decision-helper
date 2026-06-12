module Main exposing (main)

import Browser

--use div, h1, text
import Html exposing (Html, div, h1, h2, p, text, input, button)
import Html.Attributes exposing (placeholder, value, class)
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
type Page
    = CurrentDecision
    | History
    | Templates
    | Settings

type alias Model =
    { currentPage : Page
    , decisionTitle : String
    , titleLocked : Bool
    , currentOption : String
    , currentOptionDescription : String
    , nextOptionId : Int
    , options : List Option
    , currentCriteria : String
    , currentCriteriaDescription : String
    , currentCriteriaWeight : String
    , nextCriteriaId : Int
    , criteriaList : List Criteria
    }

type alias Option =
    { id : Int
    , name : String
    , description : String
    , scores : List Score
    }

type alias Score = 
    { criteriaID : Int
    , value : Int
    }

type alias Criteria = 
    { id : Int
    , name : String
    , description : String
    , weight : Int
    }

init : Model

init = 
    { currentPage = CurrentDecision
    , decisionTitle = "" 
    , titleLocked = False
    , currentOption = ""
    , currentOptionDescription = ""
    , nextOptionId = 1
    , options = []
    , currentCriteria = ""
    , currentCriteriaDescription = ""
    , currentCriteriaWeight = "1"
    , nextCriteriaId = 1
    , criteriaList = []
    }


--UPDATE, changes data
type Msg
    = ChangePage Page
    | UpdateDecisionTitle String
    | SaveDecisionTitle
    | EditDecisionTitle
    | UpdateCurrentOption String
    | UpdateCurrentOptionDescription String
    | AddOption --User clicked add option button
    | DeleteOption Int
    | UpdateCurrentCriteria String
    | UpdateCurrentCriteriaDescription String
    | UpdateCurrentCriteriaWeight String
    | AddCriteria --User clicks add criteria button
    | DeleteCriteria Int

update : Msg -> Model -> Model
update msg model = 
    case msg of
        ChangePage page ->
            { model | currentPage = page }
        UpdateDecisionTitle newTitle ->
            {model | decisionTitle = newTitle}
        SaveDecisionTitle -> 
            if String.trim model.decisionTitle /= "" then
                {model | titleLocked = True}
            else
                model
        EditDecisionTitle ->
            { model | titleLocked = False }
        UpdateCurrentOption newOptionName -> 
            { model | currentOption = newOptionName }
        UpdateCurrentOptionDescription newOptionDescription ->
            { model | currentOptionDescription = newOptionDescription}
        AddOption ->
            if String.trim model.currentOption /= "" then
                let
                    scoresForNewOption = 
                        List.map
                            (\criteria ->
                                { criteriaID = criteria.id
                                , value = 0
                                }
                            )
                            model.criteriaList

                    newOption = 
                        { id = model.nextOptionId
                        , name = model.currentOption
                        , description = model.currentOptionDescription
                        , scores = scoresForNewOption
                        }
                in
                { model
                    | options = newOption :: model.options
                    , currentOption = ""
                    , currentOptionDescription = ""
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
            { model | currentCriteria = newCriteria}
        UpdateCurrentCriteriaDescription newCriteriaDescription ->
            { model | currentCriteriaDescription = newCriteriaDescription}
        UpdateCurrentCriteriaWeight newWeight ->
            { model | currentCriteriaWeight = newWeight}
        AddCriteria ->
            if String.trim model.currentCriteria /= "" then
                model
            else
                case String.toInt model.currentCriteriaWeight of
                    Just weight ->
                        let 
                            newCriteria =
                                { id = model.nextCriteriaId
                                , name = model.currentCriteria
                                , description = model.currentCriteriaDescription
                                , weight = weight
                                }

                            updatedOptions = 
                                List.map
                                    (\option ->
                                        { option
                                            | scores =
                                                { criteriaID = newCriteria.id
                                                , value = 0
                                                }
                                                :: option.scores
                                        }
                                    )
                                    model.options
                        in
                        {model 
                            | criteriaList = newCriteria :: model.criteriaList
                            , options = updatedOptions
                            , currentCriteria = ""
                            , currentCriteriaDescription = ""
                            , currentCriteriaWeight = "1"
                            , nextCriteriaId = model.nextCriteriaId + 1
                        }
                    Nothing ->
                        model
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
    div [ class "app-container" ]
        [ viewSidebar model
        , viewContent model
        , viewDecisionPage model
        ]

viewContent : Model -> Html Msg
viewContent model =
    case model.currentPage of
        CurrentDecision ->
            viewDecisionPage model
        History ->
            viewHistoryPage model
        Templates ->
            viewTemplatesPage model
        Settings ->
            viewSettingsPage model

viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "sidebar" ]
        [ h2 [] [ text "DecideWise"]
        , p [] [ text "Better decisions, less doubt."]
        , button [ class "newDecisionBtn"] [ text "+ New Decision"]
        , button 
            [ onClick (ChangePage CurrentDecision) ]
            [ text "Current Decision"]
        , button 
            [ onClick (ChangePage History) ]
            [ text "History (Coming Soon)"]
        , button 
            [ onClick (ChangePage Templates) ]
            [ text "Templates (Coming Soon)"]
        , button 
            [ onClick (ChangePage Settings) ]
            [ text "Settings (Coming Soon)"]
        ]

viewDecisionPage : Model -> Html Msg
viewDecisionPage model =
    div [ class "content" ]
        [ h1 [] [ text "Decision Helper" ]
        , if model.titleLocked then 
            div []
                [ h2 [] [ text model.decisionTitle ]
                , button [onClick EditDecisionTitle] [text "Edit Decision"]
                ]
        else
            div []
                [ input 
                    [ placeholder "What decision are you making?"
                    , value model.decisionTitle
                    , onInput UpdateDecisionTitle] []
                , button
                    [ onClick SaveDecisionTitle]
                    [text "Save Decision"]
                ]
        , p [] [text "Created date here"]
        , p [] [text "No of Criteria here"]
        , p [] [text "No of Options here"]
        , h2 [] [text "Option:"]
        , input 
            [ placeholder "Option Name"
            , value model.currentOption
            , onInput UpdateCurrentOption] []
        , input 
            [ placeholder "Option Description (optional)"
            , value model.currentOptionDescription
            , onInput UpdateCurrentOptionDescription] []
        , button [ onClick AddOption ] [ text "Add Option"]
        , h2 [] [text "Options:"]
        , div [] 
            (List.map 
                (\option ->
                    div []
                        [ h2 [] [ text option.name ]
                        , p [] [ text option.description ]
                        , button 
                            [ onClick (DeleteOption option.id)] 
                            [ text "Delete" ]
                        ]
                )  
                model.options
            ) --transform each item in options list to a paragraph
        , h2 [] [text "Criteria:"]
        , input 
            [ placeholder "Criteria Name"
            , value model.currentCriteria
            , onInput UpdateCurrentCriteria] []
        , input 
            [ placeholder "Criteria Description (optional)"
            , value model.currentCriteriaDescription
            , onInput UpdateCurrentCriteriaDescription] []
        , input
            [ placeholder "Criteria Weight"
            , value model.currentCriteriaWeight
            , onInput UpdateCurrentCriteriaWeight] []
        , button [ onClick AddCriteria ] [text "Add Criteria"]
        , div []
            (List.map 
                (\criteria -> 
                    div []
                        [ h2 [] [ text criteria.name]
                        , p [] [ text criteria.description]
                        , p [] [ text (String.fromInt(criteria.weight))]
                        , button 
                            [ onClick (DeleteCriteria criteria.id)] 
                            [text "Delete"]
                        ]
                ) 
                model.criteriaList
            )
        ]

viewHistoryPage : Model -> Html Msg
viewHistoryPage model =
    div [ class "content" ] [ text "history page" ]

viewTemplatesPage : Model -> Html Msg
viewTemplatesPage model =
    div [ class "content" ] [ text "templates page" ]

viewSettingsPage : Model -> Html Msg
viewSettingsPage model =
    div [ class "content"]  [ text "settings page" ]