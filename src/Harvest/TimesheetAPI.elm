module Harvest.TimesheetAPI
    exposing
        ( DayEntry
        , dayEntriesDecoder
        , dayEntryDecoder
        , createEntry
        , getEntriesForCurrentDay
        , getEntriesForDayOfYear
        , getEntryById
        , deleteEntry
        , toggleEntry
        , updateEntry
        )

{-| Warpper around Harvest Timesheet API

# DayEntry
@docs DayEntry

# Timesheet API
@docs createEntry, getEntriesForCurrentDay, getEntriesForDayOfYear, getEntryById, deleteEntry, toggleEntry, updateEntry

# DayEntry decoders
@docs dayEntriesDecoder, dayEntryDecoder

-}

import Date exposing (Date)
import Date.Extra exposing (toFormattedString)
import Json.Encode.Extra as JEE exposing (maybe)
import Json.Encode as JE
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)


-- TimesheetAPI


{-| Representation of a day entry.
-}
type alias DayEntry =
    { id : Int
    , userId : Int
    , projectId : Int
    , taskId : Int
    , notes : Maybe String
    , spentAt : Date
    , hours : Float
    , project : Maybe String
    , client : Maybe String
    , task : Maybe String
    , adjustmentRecord : Maybe Bool
    , isBilled : Maybe Bool
    , isClosed : Maybe Bool
    , timerStartedAt : Maybe Date
    , hoursWithTimer : Maybe Float
    , hoursWithoutTimer : Maybe Float
    , startedAt : Maybe Date
    , endedAt : Maybe Date
    , createdAt : Date
    , updatedAt : Date
    }


{-| Representation of a simple day entry. Used with `createEntry` method
-}
type alias SimpleDayEntry =
    { notes : Maybe String
    , projectId : Int
    , taskId : Int
    , spentAt : Date
    , hours : Maybe Float
    , startedAt : Maybe Date
    , endedAt : Maybe Date
    }


{-|
Creating An Entry

POST https://YOURACCOUNT.harvestapp.com/daily/add

HTTP Response: 201 Created
-}
createEntry : String -> String -> SimpleDayEntry -> Request DayEntry
createEntry accountId token entry =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/daily/add?access_token=" ++ token
    in
        request
            { method = "POST"
            , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
            , url = url
            , body = jsonBody <| encodeSimpleDayEntry entry
            , expect = expectJson dayEntryDecoder
            , timeout = Nothing
            , withCredentials = False
            }


{-|
Retrieve Entries For The Current Day. Only tracked time, no assignments

GET https://YOURACCOUNT.harvestapp.com/daily?slim=1
-}
getEntriesForCurrentDay : String -> String -> Request (List DayEntry)
getEntriesForCurrentDay accountId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily?slim=1&access_token=" ++ token
        , body = emptyBody
        , expect = expectJson dayEntriesDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Retrieve Entries For A Specific Date

GET https://YOURACCOUNT.harvestapp.com/daily/{DAY_OF_THE_YEAR}/{YEAR}

Allowed parameters:

of_user e.g. of_user=123456

-}
getEntriesForDayOfYear : String -> Int -> Int -> String -> Maybe Int -> Request (List DayEntry)
getEntriesForDayOfYear accountId day year token userId =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = getUrlForCurrentDay accountId day year token userId
        , body = emptyBody
        , expect = expectJson dayEntriesDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Retrieving A Single Entry

GET https://YOURACCOUNT.harvestapp.com/daily/show/{DAY_ENTRY_ID}

HTTP Response: 200 OK
-}
getEntryById : String -> Int -> String -> Request DayEntry
getEntryById accountId entryId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/show/" ++ (toString entryId) ++ "&access_token=" ++ token
        , body = emptyBody
        , expect = expectJson dayEntryDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Deleting An Entry

DELETE https://YOURACCOUNT.harvestapp.com/daily/delete/{DAY_ENTRY_ID}

HTTP Response: 200 OK
-}
deleteEntry : String -> Int -> String -> Request String
deleteEntry accountId entryId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/" ++ (toString entryId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Toggling A Timer

GET https://YOURACCOUNT.harvestapp.com/daily/timer/{DAY_ENTRY_ID}

HTTP Response: 200 OK
-}
toggleEntry : String -> Int -> String -> Request String
toggleEntry accountId entryId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/timer/" ++ (toString entryId) ++ "&access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Decoders -}


{-| Decode a JSON list of day entries into a `List DayEntry`.
-}
dayEntriesDecoder : Decoder (List DayEntry)
dayEntriesDecoder =
    list (field "day_entry" dayEntryDecoder)


{-| Decode a JSON day entry into a `DayEntry`.
-}
dayEntryDecoder : Decoder DayEntry
dayEntryDecoder =
    decode DayEntry
        |> required "id" int
        |> required "user_id" int
        |> required "project_id" int
        |> required "task_id" int
        |> required "notes" (nullable string)
        |> required "spent_at" date
        |> required "hours" float
        |> required "project" (nullable string)
        |> required "client" (nullable string)
        |> required "task" (nullable string)
        |> required "adjustment_record" (nullable bool)
        |> required "is_closed" (nullable bool)
        |> required "is_billed" (nullable bool)
        |> required "timer_started_at" (nullable date)
        |> required "hours_with_timer" (nullable float)
        |> required "hours_without_timer" (nullable float)
        |> required "started_at" (nullable date)
        |> required "tended_at" (nullable date)
        |> required "created_at" date
        |> required "updated_at" date


{-|
Updating An Entry

POST https://YOURACCOUNT.harvestapp.com/daily/update/{DAY_ENTRY_ID}

HTTP Response: 200 OK
-}
updateEntry : String -> DayEntry -> String -> Request String
updateEntry accountId entry token =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/update/" ++ (toString entry.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeDayEntry entry
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Helpers -}


encodeSimpleDayEntry : SimpleDayEntry -> JE.Value
encodeSimpleDayEntry c =
    JE.object
        [ ( "notes", JEE.maybe JE.string c.notes )
        , ( "project_id", JE.int c.projectId )
        , ( "task_id", JE.int c.taskId )
        , ( "spent_at", JE.string (toFormattedString "yyyy-MM-dd" c.spentAt) )
        , ( "hours", JEE.maybe JE.float c.hours )
        , ( "started_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        , ( "ended_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        ]


extractTime : Date -> String
extractTime aDate =
    toFormattedString "h:mm a" aDate


timeOfDay : Maybe Date -> Maybe String
timeOfDay aDate =
    case aDate of
        Just d ->
            Just (extractTime d)

        Nothing ->
            Nothing


encodeDayEntry : DayEntry -> JE.Value
encodeDayEntry c =
    JE.object
        [ ( "notes", JEE.maybe JE.string c.notes )
        , ( "project_id", JE.int c.projectId )
        , ( "task_id", JE.int c.taskId )
        , ( "spent_at", JE.string (toFormattedString "yyyy-MM-dd" c.spentAt) )
        , ( "started_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        , ( "ended_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        ]


getUrlForCurrentDay : String -> Int -> Int -> String -> Maybe Int -> String
getUrlForCurrentDay accountId day year token userId =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/daily/" ++ (toString day) ++ "/" ++ (toString year) ++ "?access_token=" ++ token
    in
        case userId of
            Just id ->
                url ++ "&of_user" ++ (toString id)

            Nothing ->
                url
