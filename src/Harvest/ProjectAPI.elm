module Harvest.ProjectAPI
    exposing
        ( Project
        , SimpleProject
        , getProject
        , getAllProjects
        , createProject
        , deleteProject
        , updateProject
        , toggleProject
        )

{-| Warpper around Harvest Project API
-}

import Date exposing (Date)
import Date.Extra exposing (toFormattedString)
import Json.Encode as JE
import Json.Encode.Extra as JEE exposing (maybe)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)
import Dict exposing (Dict)


{-| Representation of a project.
-}
type alias Project =
    { id : Int
    , clientId : Int
    , name : String
    , code : Maybe String
    , active : Bool
    , billable : Bool
    , billBy : String
    , hourlyRate : Maybe Float
    , budget : Maybe Float
    , budgetBy : String
    , notifyWhenOverBudget : Bool
    , overBudgetNotificationPercentage : Float
    , overBudgetNotifiedAt : Maybe Date
    , showBudgetToAll : Bool
    , createdAt : Date
    , updatedAt : Date
    , startsOn : Maybe Date
    , endsOn : Maybe Date
    , estimate : Maybe Float
    , estimateBy : String
    , hintEarliestRecordAt : Maybe Date
    , hintLatestRecordAt : Maybe Date
    , notes : Maybe String
    , costBudget : Maybe Float
    , costBudgetIncludeExpenses : Bool
    }


{-| Simple representation of a project. Used with `createProject` method
-}
type alias SimpleProject =
    { clientId : Int
    , name : String
    , active : Bool
    }


{-| Decode a JSON project into a `Prpject`.
-}
projectDecoder : Decoder Project
projectDecoder =
    decode Project
        |> required "id" int
        |> required "client_id" int
        |> required "name" string
        |> required "code" (nullable string)
        |> required "active" bool
        |> required "billable" bool
        |> required "bill_by" string
        |> required "hourly_rate" (nullable float)
        |> required "budget" (nullable float)
        |> required "budget_by" string
        |> required "notify_when_over_budget" bool
        |> required "over_budget_notification_percentage" float
        |> required "over_budget_notified_at" (nullable date)
        |> required "show_budget_to_all" bool
        |> required "created_at" date
        |> required "updated_at" date
        |> required "starts_on" (nullable date)
        |> required "ends_on" (nullable date)
        |> required "estimate" (nullable float)
        |> required "estimate_by" string
        |> required "hint_earliest_record_at" (nullable date)
        |> required "hint_latest_record_at" (nullable date)
        |> required "notes" (nullable string)
        |> required "cost_budget" (nullable float)
        |> required "cost_budget_include_expenses" bool


{-| Decode a JSON list of projects into a `List Project`.
-}
projectsDecoder : Decoder (List Project)
projectsDecoder =
    list (field "project" projectDecoder)


{-|
Show A Project

GET https://YOURACCOUNT.harvestapp.com/projects/{PROJECTID}

HTTP Response: 200 OK
-}
getProject : String -> String -> String -> Request Project
getProject accountId projectId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ projectId ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson projectDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Show All Projects

GET https://YOURACCOUNT.harvestapp.com/projects

HTTP Response: 200 OK

Allowed parameters:

client_id: client={CLIENTID}
updated_since: updated_since=2015-03-25+18%3A30
-}
getAllProjects : String -> String -> Dict String String -> Request (List Project)
getAllProjects accountId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrl accountId token params
        , body = emptyBody
        , expect = expectJson projectsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Create A New Project

POST https://YOURACCOUNT.harvestapp.com/projects

HTTP Response: 201 Created
-}
createProject : String -> String -> SimpleProject -> Request String
createProject accountId token project =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/projects?access_token=" ++ token
    in
        request
            { method = "POST"
            , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
            , url = url
            , body = jsonBody <| encodeSimpleProject project
            , expect = expectString
            , timeout = Nothing
            , withCredentials = False
            }


{-|
Delete A Project

DELETE https://YOURACCOUNT.harvestapp.com/projects/{PROJECTID}
-}
deleteProject : String -> Int -> String -> Request String
deleteProject accountId projectId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Update An Existing Project

PUT https://YOURACCOUNT.harvestapp.com/projects/{PROJECTID}
-}
updateProject : String -> Project -> String -> Request String
updateProject accountId project token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString project.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeProject project
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
(De)Activate An Existing Project

PUT https://YOURACCOUNT.harvestapp.com/projects/{PROJECTID}/toggle
-}
toggleProject : String -> Int -> String -> Request String
toggleProject accountId projectId token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/toggle?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Helper methods -}


encodeProject : Project -> JE.Value
encodeProject p =
    JE.object
        [ ( "project"
          , JE.object
                [ ( "client_id", JE.int p.clientId )
                , ( "name", JE.string p.name )
                , ( "active", JE.bool p.active )
                , ( "billable", JE.bool p.billable )
                , ( "bill_by", JE.string p.billBy )
                , ( "hourly_rate", JEE.maybe JE.float p.hourlyRate )
                , ( "budget", JEE.maybe JE.float p.budget )
                , ( "budget_by", JE.string p.budgetBy )
                , ( "notify_when_over_budget", JE.bool p.notifyWhenOverBudget )
                , ( "over_budget_notification_percentage", JE.float p.overBudgetNotificationPercentage )
                , ( "over_budget_notified_at", JEE.maybe JE.string (stringifyDate p.overBudgetNotifiedAt) )
                , ( "show_budget_to_all", JE.bool p.showBudgetToAll )
                  --, ( "created_at", JE.date p.createdAt )
                  -- , ( "updated_at", JE.date p.updatedAt )
                , ( "starts_on", JEE.maybe JE.string (stringifyDate p.startsOn) )
                , ( "ends_on", JEE.maybe JE.string (stringifyDate p.endsOn) )
                , ( "estimate", JEE.maybe JE.float p.estimate )
                , ( "estimate_by", JE.string p.estimateBy )
                , ( "hint_earliest_record_at", JEE.maybe JE.string (stringifyDate p.hintEarliestRecordAt) )
                , ( "hint_latest_record_at", JEE.maybe JE.string (stringifyDate p.hintLatestRecordAt) )
                , ( "notes", JEE.maybe JE.string p.notes )
                , ( "cost_budget", JEE.maybe JE.float p.costBudget )
                , ( "cost_budget_include_expenses", JE.bool p.costBudgetIncludeExpenses )
                ]
          )
        ]


dateAsString : Date -> String
dateAsString aDate =
    toFormattedString "yyyy-MM-dd" aDate


stringifyDate : Maybe Date -> Maybe String
stringifyDate aDate =
    case aDate of
        Just d ->
            Just (dateAsString d)

        Nothing ->
            Nothing


encodeSimpleProject : SimpleProject -> JE.Value
encodeSimpleProject np =
    JE.object
        [ ( "project"
          , JE.object
                [ ( "client_id", JE.int np.clientId )
                , ( "name", JE.string np.name )
                , ( "active", JE.bool np.active )
                ]
          )
        ]


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/projects?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
