module Harvest.WhoAmI
    exposing
        ( ProjectManager
        , User
        , Modules
        , Company
        , WhoAmI
        , getUserInfo
        )

{-| Warpper around Harvest Who Am I API
-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http exposing (..)


{-| Representation of a project manager.
-}
type alias ProjectManager =
    { isProjectManager : Bool
    , canSeeRates : Bool
    , canCreateProjects : Bool
    , canCreateInvoices : Bool
    }


{-| Representation of a user.
-}
type alias User =
    { timezone : String
    , timezoneIdentifier : String
    , timezoneUtcOffset : Int
    , id : Int
    , email : String
    , admin : Bool
    , firstName : String
    , lastName : String
    , avatarUrl : String
    , projectManager : ProjectManager
    , timestampTimers : Bool
    }


{-| Representation of modules.
-}
type alias Modules =
    { expenses : Bool
    , invoices : Bool
    , estimates : Bool
    , approval : Bool
    }


{-| Representation of a company.
-}
type alias Company =
    { baseUri : String
    , fullDomain : String
    , name : String
    , active : Bool
    , weekStartDay : String
    , timeFormat : String
    , clock : String
    , decimalSymbol : String
    , colorScheme : String
    , modules : Modules
    , thousandsSeparator : String
    , planType : String
    }


{-| Representation of the user and company.
-}
type alias WhoAmI =
    { user : User
    , company : Company
    }


{-|
This call will return user and account information for the currently authenticated user.

GET https://YOURACCOUNT.harvestapp.com/account/who_am_i
-}
getUserInfo : String -> String -> Request WhoAmI
getUserInfo accountId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = urlWhoAmI accountId token
        , body = emptyBody
        , expect = expectJson whoAmI
        , timeout = Nothing
        , withCredentials = False
        }



{- Helpers -}


urlWhoAmI : String -> String -> String
urlWhoAmI accountId token =
    "https://" ++ accountId ++ ".harvestapp.com/account/who_am_i?access_token=" ++ token



{- decoders -}


{-| Decode a JSON project manager into a `ProjectManager`.
-}
projectManager : Decoder ProjectManager
projectManager =
    decode ProjectManager
        |> required "is_project_manager" bool
        |> required "can_see_rates" bool
        |> required "can_create_projects" bool
        |> required "can_create_invoices" bool


{-| Decode a JSON user into a `User`.
-}
user : Decoder User
user =
    decode User
        |> required "timezone" string
        |> required "timezone_identifier" string
        |> required "timezone_utc_offset" int
        |> required "id" int
        |> required "email" string
        |> required "admin" bool
        |> required "first_name" string
        |> required "last_name" string
        |> required "avatar_url" string
        |> required "project_manager" projectManager
        |> required "timestamp_timers" bool


{-| Decode a JSON modules into a `Modules`.
-}
modules : Decoder Modules
modules =
    decode Modules
        |> required "expenses" bool
        |> required "invoices" bool
        |> required "estimates" bool
        |> required "approval" bool


{-| Decode a JSON company into a `Company`.
-}
company : Decoder Company
company =
    decode Company
        |> required "base_uri" string
        |> required "full_domain" string
        |> required "name" string
        |> required "active" bool
        |> required "week_start_day" string
        |> required "time_format" string
        |> required "clock" string
        |> required "decimal_symbol" string
        |> required "color_scheme" string
        |> required "modules" modules
        |> required "thousands_separator" string
        |> required "plan_type" string


{-| Decode a JSON "Who Am I" into a `WhoAmI`.
-}
whoAmI : Decoder WhoAmI
whoAmI =
    decode WhoAmI
        |> required "user" user
        |> required "company" company
