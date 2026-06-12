module Main exposing (main)

import Browser

--use div, h1, text
import Html exposing (Html, div, h1, h2, h3, p, text, input, button)
import Html.Attributes exposing (placeholder, value, class)
import Html.Events exposing (onInput, onClick)

import String

import Html exposing (option)


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

type DecisionTab
    = OverviewTab
    | CriteriaTab
    | OptionsTab
    | ProsAndConsTab
    | ScoringTab
    | ResultTab

type alias Model =
    { currentPage : Page
    , currentTab : DecisionTab
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
    , currentTab = OverviewTab
    , decisionTitle = "" 
    , titleLocked = False
    , currentOption = ""
    , currentOptionDescription = ""
    , nextOptionId = 1
    , options = []
    , currentCriteria = ""
    , currentCriteriaDescription = ""
    , currentCriteriaWeight = ""
    , nextCriteriaId = 1
    , criteriaList = []
    }


--UPDATE, changes data
type Msg
    = ChangePage Page
    | ChangeTab DecisionTab
    | UpdateDecisionTitle String
    | SaveDecisionTitle
    | EditDecisionTitle
    | UpdateCurrentOption String
    | UpdateCurrentOptionDescription String
    | AddOption --User clicked add option button
    | UpdateScore Int Int String --optionId, criteriaId, value
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
        ChangeTab tab ->
            { model | currentTab = tab }
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
        UpdateScore optionId criteriaId scoreValue ->
            case String.toInt scoreValue of
                Just newScore ->
                    { model
                    | options =
                        List.map
                            (\option ->
                                if option.id == optionId then
                                    { option
                                        | scores =
                                            List.map
                                                (\score ->
                                                    if score.criteriaID == criteriaId then
                                                        { score | value = newScore }
                                                    else
                                                        score
                                                )
                                                option.scores
                                    }
                                else
                                    option
                            )
                            model.options
                    }
                Nothing ->
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
            if String.trim model.currentCriteria == "" then
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
                            , currentCriteriaWeight = ""
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



--HELPERS
getCriteriaById : Int -> List Criteria -> Maybe Criteria
getCriteriaById criteriaId criteriaList =
    List.head
        (List.filter
            (\criteria -> criteria.id == criteriaId)
            criteriaList
        )

getCriteriaName : Int -> List Criteria -> String
getCriteriaName criteriaId criteriaList =
    case getCriteriaById criteriaId criteriaList of
        Just criteria ->
            criteria.name
        Nothing ->
            "Unknown Criteria"

getCriteriaWeight : Int -> List Criteria -> Int
getCriteriaWeight criteriaId criteriaList =
    case getCriteriaById criteriaId criteriaList of
        Just criteria ->
            criteria.weight
        Nothing ->
            0

calculateOptionScore : Option -> List Criteria -> Int
calculateOptionScore option criteriaList =
    List.sum
        (List.map
            (\score ->
                score.value
                    * getCriteriaWeight score.criteriaID criteriaList
            )
            option.scores
        )

getBestOption : List Option -> List Criteria -> Maybe Option
getBestOption options criteriaList =
    case options of
        [] ->
            Nothing
        first :: rest ->
            Just
                (List.foldl
                    (\option currentBest ->
                        if calculateOptionScore option criteriaList
                            > calculateOptionScore currentBest criteriaList then
                            
                            option
                        
                        else
                            currentBest
                    )
                    first
                    rest
                )



--VIEW, turns data into HTML
view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ viewSidebar model
        , viewContent model
        ]

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


viewDecisionPage : Model -> Html Msg
viewDecisionPage model = 
    div [ class "content" ]
        [ viewDecisionTabs model
        , viewDecisionContent model]

viewDecisionTabs : Model -> Html Msg
viewDecisionTabs model =
    div [ class "decision-title-bar"] [
        div [ class "decision-title" ]
            [ if model.titleLocked then 
                div []
                    [ h1 [] [ text model.decisionTitle ]
                    , button 
                        [ class "secondary-btn" 
                        , onClick EditDecisionTitle] 
                        [text "Edit Decision"]
                    ]
            else
                div []
                    [ input 
                        [ placeholder "What decision are you making?"
                        , value model.decisionTitle
                        , onInput UpdateDecisionTitle] []
                    , button
                        [ class "secondary-btn"
                        , onClick SaveDecisionTitle]
                        [text "Save Decision"]
                    ]
            , div [ class "decision-details" ] 
                [ p [] [text "Created date here"]
                , p [] [text ("Criteria: " ++ String.fromInt (List.length model.criteriaList))]
                , p [] [text ("Options: " ++ String.fromInt (List.length model.options))]
                ]
            ]
        , div [ class "tab-bar" ]
            [ button 
                [ onClick (ChangeTab OverviewTab) ]
                [ text "Overview" ] 
            , button 
                [ onClick (ChangeTab CriteriaTab) ]
                [ text "Criteria" ] 
            , button 
                [ onClick (ChangeTab OptionsTab) ]
                [ text "Options" ] 
            , button 
                [ onClick (ChangeTab ProsAndConsTab) ]
                [ text "Pros & Cons" ] 
            , button
                [ onClick (ChangeTab ScoringTab) ]
                [ text "Scoring" ]
            , button 
                [ onClick (ChangeTab ResultTab) ]
                [ text "Result" ] 
            ]
    ]

viewDecisionContent : Model -> Html Msg
viewDecisionContent model =
    case model.currentTab of
        OverviewTab ->
            viewOverviewTab model
        CriteriaTab ->
            viewCriteriaTab model
        OptionsTab ->
            viewOptionsTab model
        ProsAndConsTab ->
            viewProsAndConsTab model
        ScoringTab ->
            viewScoringTab model
        ResultTab ->
            viewResultTab model

viewOverviewTab : Model -> Html Msg
viewOverviewTab model =
    div [ class "decision-content" ]
        [ div [ class "main-overview" ] 
            [ div [ class "criteria-overview"] 
                [ h2 [] [ text "Criteria & Weights" ]
                , if List.isEmpty model.criteriaList then
                    p [ class "empty-state" ]
                        [ text "No criteria added yet." ]
                else
                    div []
                        (List.map 
                            (\criteria -> 
                                div [ class "criteria-row" ]
                                    [ div [ class "criteria-info" ] 
                                        [h3 [] [ text criteria.name]
                                        , p [] [ text criteria.description]
                                        ]
                                    , div [ class "weight-badge"]
                                        [ text (String.fromInt criteria.weight) ]
                                    ]
                            ) 
                            model.criteriaList
                        )
                ]
            , div [ class "options-overview"] 
                [ h2 [] [ text "Options" ]
                , if List.isEmpty model.options then
                    p [ class "empty-state" ]
                        [ text "No options added yet." ]
                else
                    div [] 
                        (List.map 
                            (\option ->
                                div [ class "option-card" ]
                                    [ h3 [] [ text option.name ]
                                    , p [] [ text option.description ]
                                    ]
                            )  
                            model.options
                        )
                , p [] [ text "Scores are calculated using weighted average." ]
                , p [] [ text "Score range: 1 (low) - 10 (high)" ]
                ]
            , div [ class "notes" ] 
                [ h3 [] [ text "Notes" ]
                , p [] [ text "Add any notes here" ]
                , button [ class "secondary-btn" ] [ text "Edit" ]
                ]
            ]
        , div [ class "recommendation-card" ] 
            [ h3 [] [ text "Recommended Choice" ]
            , case getBestOption model.options model.criteriaList of
                Just option ->
                    h1 [] [ text option.name ]
                Nothing ->
                    h2 [] [ text "No Recommendation Yet" ]            
            , case getBestOption model.options model.criteriaList of
                Just option ->
                    p [] [ text (option.name ++ " scores highest overall based on your criteria and weights") ]
                Nothing ->
                    p [] [ text "" ]
            ]
        ]

viewOptionsTab : Model -> Html Msg
viewOptionsTab model =
    div [ class "decision-content"] 
        [ h2 [] [text "Option:"]
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
    ]

viewCriteriaTab : Model -> Html Msg
viewCriteriaTab model =
    div [ class "decision-content"] 
        [ h2 [] [text "Criteria:"]
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
                        , p [] [ text (String.fromInt criteria.weight)]
                        , button 
                            [ onClick (DeleteCriteria criteria.id)] 
                            [text "Delete"]
                        ]
                ) 
                model.criteriaList
            )
    ]

viewProsAndConsTab : Model -> Html Msg
viewProsAndConsTab model =
    div [ class "decision-content"] []

viewScoringTab : Model -> Html Msg
viewScoringTab model =
    div [ class "decision-content" ]
        (List.map
            (\option ->
                div [ class "option-score-card" ]
                    [ h2 [] [ text option.name ]

                    , div []
                        (List.map
                            (\score ->
                                div [ class "score-row" ]
                                    [ p []
                                        [ text
                                            (getCriteriaName
                                                score.criteriaID
                                                model.criteriaList
                                            )
                                        ]

                                    , input
                                        [ value (String.fromInt score.value)
                                        , onInput
                                            (\newValue ->
                                                UpdateScore
                                                    option.id
                                                    score.criteriaID
                                                    newValue
                                            )
                                        ]
                                        []
                                    ]
                            )
                            option.scores
                        )
                    ]
            )
            model.options
        )
viewResultTab : Model -> Html Msg
viewResultTab model =
    div [ class "decision-content"] 
        [ h1 [] [ text "Results" ]
        , case getBestOption model.options model.criteriaList of
            Just winner ->
                div []
                    [ h2 [] [ text ("Winner: " ++ winner.name) ]
                    ]
            Nothing ->
                text ""
        , div[]
            (List.map
                (\option ->
                    div []
                        [ h3 [] [text option.name ]
                        , p [] [ text 
                            ("Score: " 
                                ++ String.fromInt 
                                    (calculateOptionScore 
                                        option 
                                        model.criteriaList
                                    )
                            )
                        ]
                        ]
                ) 
                model.options
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