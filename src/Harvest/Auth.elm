module Harvest.Auth exposing (checkAccessTokenAvailable, authUrl)

{-| Support for Authentication

# Utility methods
@docs authUrl, checkAccessTokenAvailable

-}

import Dict
import Http exposing (..)


-- Harvest URLs


{-| create Harvest authorization API using account name (accountId), id of your client app (clientId) and redirectUrl

Usage:
`authUrl accountId clientId redirectUrl`
-}
authUrl : String -> String -> String -> String
authUrl accountId clientId redirectUrl =
    "https://"
        ++ accountId
        ++ ".harvestapp.com/oauth2/authorize?response_type=token&immediate=true&approval_prompt=auto&client_id="
        ++ clientId
        ++ "&redirect_uri="
        ++ (Http.encodeUri redirectUrl)



-- Token


{-| check if access_token is present in the url

Usage:
`checkAccessTokenAvailable urlHashToParse authenticationUrl`
-}
checkAccessTokenAvailable : String -> String -> Result String String
checkAccessTokenAvailable urlHashToParse authenticationUrl =
    case Dict.get "access_token" (parseUrlParams urlHashToParse) of
        Just a ->
            Ok a

        Nothing ->
            Err authenticationUrl



{- Helpers -}


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
    s
        |> String.dropLeft 1
        |> String.split "&"
        |> List.map parseSingleParam
        |> Dict.fromList


parseSingleParam : String -> ( String, String )
parseSingleParam p =
    let
        s =
            String.split "=" p
    in
        case s of
            [ key, val ] ->
                ( key, val )

            _ ->
                ( "", "" )
