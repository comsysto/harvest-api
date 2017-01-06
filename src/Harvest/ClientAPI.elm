module Harvest.ClientAPI
    exposing
        ( Client
        , clientsDecoder
        , clientDecoder
        , getAllClients
        , getClient
        , createClient
        , updateClient
        , toggleClient
        , deleteClient
        , Contact
        , contactsDecoder
        , contactDecoder
        , getAllContacts
        , getAllContactsForClient
        , getClientContact
        , createContact
        , updateContact
        , deleteContact
        )

{-| Warpper around Harvest Client API
-}

import Date exposing (Date)
import Json.Encode as JE
import Json.Encode.Extra as JEE exposing (maybe)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)
import Dict exposing (Dict)


{-| Representation of a client.
-}
type alias Client =
    { id : Int
    , name : String
    , active : Bool
    , currency : String
    , currencySymbol : String
    , highriseId : Maybe Int
    , cacheVersion : Int
    , details : Maybe String
    , defaultInvoiceTimeframe : Maybe String
    , lastInvoiceKind : Maybe String
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


{-| Representation of a conact.
-}
type alias Contact =
    { id : Int
    , clientId : Int
    , firstName : String
    , lastName : String
    , email : Maybe String
    , phoneOffice : Maybe String
    , phoneMobile : Maybe String
    , fax : Maybe String
    , title : Maybe String
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


{-|
Get All Clients

GET https://YOURACCOUNT.harvestapp.com/clients
-}
getAllClients : String -> String -> Request (List Client)
getAllClients accountId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/clients?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson clientsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Get A Single Client

GET https://YOURACCOUNT.harvestapp.com/clients/{CLIENTID}
-}
getClient : String -> Int -> String -> Request Client
getClient accountId clientId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/clients/" ++ (toString clientId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson clientDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Create A New Client

POST https://YOURACCOUNT.harvestapp.com/clients

HTTP Response: 201 Created, along with Location /clients/{NEWCLIENTID}
-}
createClient : String -> String -> Client -> Request String
createClient accountId token client =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/clients?access_token=" ++ token
        , body = jsonBody <| encodeClient client
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Update A Client

PUT https://YOURACCOUNT.harvestapp.com/clients/{CLIENTID}

HTTP Response: 200 OK, along with Location /clients/{CLIENTID}
-}
updateClient : String -> Client -> String -> Request String
updateClient accountId client token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/clients/" ++ (toString client.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeClient client
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Activate Or Deactivate An Existing Client

POST https://YOURACCOUNT.harvestapp.com/clients/{CLIENTID}/toggle

HTTP Response: 200 OK, along with Location /clients/{CLIENTID}
-}
toggleClient : String -> Int -> String -> Request String
toggleClient accountId clientId token =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/clients/" ++ (toString clientId) ++ "/toggle?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Delete A Client

DELETE https://YOURACCOUNT.harvestapp.com/clients/{CLIENTID}

HTTP Response: 200 OK
-}
deleteClient : String -> Int -> String -> Request String
deleteClient accountId clientId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/clients/" ++ (toString clientId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Contacts -}


{-|
Get All Contacts

GET https://YOURACCOUNT.harvestapp.com/contacts

HTTP Response: 200 OK

Allowed parameters:

updated_since: updated_since=2010-09-25+18%3A30
-}
getAllContacts : String -> String -> Dict String String -> Request (List Contact)
getAllContacts accountId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrl accountId token params
        , body = emptyBody
        , expect = expectJson contactsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Get All Contacts For A Client

GET https://YOURACCOUNT.harvestapp.com/clients/{CLIENTID}/contacts

HTTP Response: 200 OK

Allowed parameters:

updated_since: updated_since=2010-09-25+18%3A30
-}
getAllContactsForClient : String -> Int -> String -> Dict String String -> Request (List Contact)
getAllContactsForClient accountId clientId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrlForClientContacts accountId clientId token params
        , body = emptyBody
        , expect = expectJson contactsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Get A Client Contact

GET https://YOURACCOUNT.harvestapp.com/contacts/{CONTACTID}

HTTP Response: 200 OK
-}
getClientContact : String -> Int -> String -> Request Contact
getClientContact accountId clientId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/contacts/" ++ (toString clientId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson contactDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Create A New Client Contact

POST https://YOURACCOUNT.harvestapp.com/contacts

HTTP Response: 201 Created

Note: Note: Only Client-ID, First-Name, and Last-Name are required.
-}
createContact : String -> String -> Contact -> Request String
createContact accountId token contact =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/contacts?access_token=" ++ token
        , body = jsonBody <| encodeContact contact
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Update A Client Contact

PUT https://YOURACCOUNT.harvestapp.com/contacts/{CONTACTID}

HTTP Response: 200 OK, along with Location /contacts/{CONTACTID}
-}
updateContact : String -> Contact -> String -> Request String
updateContact accountId contact token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/contacts/" ++ (toString contact.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeContact contact
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Delete A Client Contact

DELETE https://YOURACCOUNT.harvestapp.com/contacts/{CONTACTID}

HTTP Response: 200 OK
-}
deleteContact : String -> Int -> String -> Request String
deleteContact accountId contactId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/contacts/" ++ (toString contactId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Decoders -}


{-| Decode a JSON list of clients into a `List Client`.
-}
clientsDecoder : Decoder (List Client)
clientsDecoder =
    list (field "client" clientDecoder)


{-| Decode a JSON client into a `Client`.
-}
clientDecoder : Decoder Client
clientDecoder =
    decode Client
        |> required "id" int
        |> required "name" string
        |> required "active" bool
        |> required "currency" string
        |> required "currency_symbol" string
        |> required "highrise_id" (nullable int)
        |> required "cache_version" int
        |> required "details" (nullable string)
        |> required "default_invoice_timeframe" (nullable string)
        |> required "last_invoice_kind" (nullable string)
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)


{-| Decode a JSON list of contacts into a `List Contact`.
-}
contactsDecoder : Decoder (List Contact)
contactsDecoder =
    list (field "contact" contactDecoder)


{-| Decode a JSON contact into a `Contact`.
-}
contactDecoder : Decoder Contact
contactDecoder =
    decode Contact
        |> required "id" int
        |> required "client_id" int
        |> required "first_name" string
        |> required "last_name" string
        |> required "email" (nullable string)
        |> required "phone_office" (nullable string)
        |> required "phone_mobile" (nullable string)
        |> required "fax" (nullable string)
        |> required "title" (nullable string)
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)



{- helpers -}


encodeClient : Client -> JE.Value
encodeClient c =
    JE.object
        [ ( "client"
          , JE.object
                [ ( "name", JE.string c.name )
                , ( "active", JE.bool c.active )
                , ( "currency", JE.string c.currency )
                , ( "currency_symbol", JE.string c.currencySymbol )
                , ( "details", JEE.maybe JE.string c.details )
                ]
          )
        ]


encodeContact : Contact -> JE.Value
encodeContact c =
    JE.object
        [ ( "contact"
          , JE.object
                [ ( "client_id", JE.int c.clientId )
                , ( "first_name", JE.string c.firstName )
                , ( "last_name", JE.string c.lastName )
                , ( "email", JEE.maybe JE.string c.email )
                , ( "phone_office", JEE.maybe JE.string c.phoneOffice )
                , ( "phone_mobile", JEE.maybe JE.string c.phoneMobile )
                , ( "fax", JEE.maybe JE.string c.fax )
                , ( "title", JEE.maybe JE.string c.title )
                ]
          )
        ]


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/contacts?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p



-- https://YOURACCOUNT.harvestapp.com/clients/{CLIENTID}/contacts


createUrlForClientContacts : String -> Int -> String -> Dict String String -> String
createUrlForClientContacts accountId clientId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/clients/" ++ (toString clientId) ++ "contacts?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
