module Harvest.InvoiceAPI
    exposing
        ( Invoice
        , invoicesDecoder
        , invoiceDecoder
        , getAllInvoices
        , getInvoice
        , deleteInvoice
        , updateInvoice
        , createInvoice
        , Message
        , messagesDecoder
        , messageDecoder
        , getMessagesForInvoice
        , deleteMessage
        , sendInvoice
        , markInvoiceAsDraft
        , markInvoiceAsSent
        , markInvoiceAsClosed
        , markInvoiceAsOpen
        , InvoiceCategory
        , invoiceCategoriesDecoder
        , invoiceCategoryDecoder
        , getInvoiceCategories
        , createInvoiceCategory
        , updateInvoiceCategory
        , deleteInvoiceCategory
        , Payment
        , paymentsDecoder
        , paymentDecoder
        , getPaymentsForInvoice
        , getPaymentForInvoice
        , deletePayment
        , createPayment
        )

{-| Warpper around Harvest Invoice API

# Invoice
@docs Invoice

# Invoice API
@docs getAllInvoices, getInvoice, deleteInvoice, updateInvoice, createInvoice

# Invoice decoders
@docs invoicesDecoder, invoiceDecoder

# Message
@docs Message

# Message API
@docs getMessagesForInvoice, deleteMessage, sendInvoice, markInvoiceAsDraft, markInvoiceAsSent, markInvoiceAsClosed, markInvoiceAsOpen

# Message decoders
@docs messagesDecoder, messageDecoder

# InvoiceCategory
@docs InvoiceCategory

# InvoiceCategory API
@docs getInvoiceCategories, createInvoiceCategory, updateInvoiceCategory, deleteInvoiceCategory

# InvoiceCategory decoders
@docs invoiceCategoriesDecoder, invoiceCategoryDecoder

# Payment
@docs Payment

# Payment API
@docs getPaymentsForInvoice, getPaymentForInvoice, deletePayment, createPayment

# Payment decoders
@docs paymentsDecoder, paymentDecoder
-}

import Date exposing (Date)
import Date.Extra exposing (toFormattedString, toUtcIsoString)
import Json.Encode as JE
import Json.Encode.Extra as JEE exposing (maybe)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)
import Dict exposing (Dict)


{-| Representation of an invoice.
-}
type alias Invoice =
    { id : Int
    , clientId : Int
    , clientName : String
    , number : Maybe Int
    , periodStart : Date
    , periodEnd : Date
    , amount : Float
    , currency : String
    , notes : Maybe String
    , kind : Maybe String
    , projectsToInvoice : Maybe String
    , issuedAt : Date
    , dueAmount : Float
    , dueAt : Date
    , dueAtHumanFormat : String
    , subject : Maybe String
    , discount : Maybe String
    , discountAmount : Maybe Float
    , purchaseOrder : String
    , tax : Float
    , taxAmount : Float
    , tax2 : Maybe Float
    , tax2Amount : Float
    , estimateId : Maybe Int
    , recurringInvoiceId : Maybe Int
    , clientKey : Maybe String
    , retainerId : Maybe Int
    , createdById : Maybe Int
    , state : Maybe String
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


{-| Representation of a message.
-}
type alias Message =
    { id : Int
    , invoiceId : Int
    , sendMeACopy : Bool
    , body : Maybe String
    , sentBy : String
    , sentByEmail : String
    , thankYou : Bool
    , subject : Maybe String
    , includePayPalLink : Bool
    , sentFomEmail : String
    , sentFrom : String
    , sendReminderOn : Maybe Date
    , fullRecipientList : Maybe String
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


{-| Representation of a simple message. Used with sendInvoice method
-}
type alias SimpleMessage =
    { invoiceId : Int
    , body : String
    , recipients : String
    , attachPdf : Bool
    , sendMeAnEmail : Bool
    , includePayPalLink : Bool
    }


{-| Representation of an invoice category.
-}
type alias InvoiceCategory =
    { id : Int
    , name : String
    , useAsService : Bool
    , useAsExpense : Bool
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


{-| Representation of a payment.
-}
type alias Payment =
    { id : Int
    , invoiceId : Int
    , amount : Float
    , paidAt : Date
    , notes : Maybe String
    , recordedBy : String
    , recordedByEmail : String
    , payPalTransactionId : Maybe Int
    , authorization : Maybe String
    , paymentGatewayId : Maybe Int
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }



{- Decoders -}


{-| Decode a JSON list of invoices into a `List Invoice`.
-}
invoicesDecoder : Decoder (List Invoice)
invoicesDecoder =
    list (field "invoice" invoiceDecoder)


{-| Decode a JSON invoice into an `Invoice`.
-}
invoiceDecoder : Decoder Invoice
invoiceDecoder =
    decode Invoice
        |> required "id" int
        |> required "client_id" int
        |> required "client_name" string
        |> required "number" (nullable int)
        |> required "period_start" date
        |> required "period_end" date
        |> required "amount" float
        |> required "currency" string
        |> required "notes" (nullable string)
        |> required "kind" (nullable string)
        |> required "projects_to_invoice" (nullable string)
        |> required "issued_at" date
        |> required "due_amount" float
        |> required "due_at" date
        |> required "due_at_human_format" string
        |> required "subject" (nullable string)
        |> required "discount" (nullable string)
        |> required "discount_amount" (nullable float)
        |> required "purchase_order" string
        |> required "tax" float
        |> required "tax_amount" float
        |> required "tax2" (nullable float)
        |> required "tax2_amount" float
        |> required "estimate_id" (nullable int)
        |> required "recurring_invoice_id" (nullable int)
        |> required "client_key" (nullable string)
        |> required "retainer_id" (nullable int)
        |> required "created_by_id" (nullable int)
        |> required "state" (nullable string)
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)


{-| Decode a JSON list of messages into a `List Message`.
-}
messagesDecoder : Decoder (List Message)
messagesDecoder =
    list (field "message" messageDecoder)


{-| Decode a JSON message into a `Message`.
-}
messageDecoder : Decoder Message
messageDecoder =
    decode Message
        |> required "id" int
        |> required "invoice_id" int
        |> required "send_me_a_copy" bool
        |> required "body" (nullable string)
        |> required "sent_by" string
        |> required "sent_by_email" string
        |> required "thank_you" bool
        |> required "subject" (nullable string)
        |> required "include_pay_pal_link" bool
        |> required "sent_from" string
        |> required "sent_from_email" string
        |> required "send_reminder_on" (nullable date)
        |> required "full_recipient_list" (nullable string)
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)


{-| Decode a JSON list of invoice categories into a `List InvoiceCategory`.
-}
invoiceCategoriesDecoder : Decoder (List InvoiceCategory)
invoiceCategoriesDecoder =
    list (field "invoice_category" invoiceCategoryDecoder)


{-| Decode a JSON invoice category into a `InvoiceCategory`.
-}
invoiceCategoryDecoder : Decoder InvoiceCategory
invoiceCategoryDecoder =
    decode InvoiceCategory
        |> required "id" int
        |> required "name" string
        |> required "use_as_service" bool
        |> required "use_as_expense" bool
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)


{-| Decode a JSON list of payments into a `List Payment`.
-}
paymentsDecoder : Decoder (List Payment)
paymentsDecoder =
    list (field "invoice" paymentDecoder)


{-| Decode a JSON payment into a `Payment`.
-}
paymentDecoder : Decoder Payment
paymentDecoder =
    decode Payment
        |> required "id" int
        |> required "invoice_id" int
        |> required "amount" float
        |> required "paid_at" date
        |> required "notes" (nullable string)
        |> required "recorded_by" string
        |> required "recorded_by_email" string
        |> required "pay_pal_transaction_id" (nullable int)
        |> required "authorization" (nullable string)
        |> required "payment_gateway_id" (nullable int)
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)


{-|
Show Recently Created Invoices
GET https://YOURACCOUNT.harvestapp.com/invoices

HTTP Response: 200 OK

Allowed parameters:
page: page=2 (first page starts with 1)
from/to: from=YYYYMMDD&to=YYYYMMDD
updated_since: updated_since=2010-09-25+18%3A30
status: status=partial (possible invoice states are [open, partial, draft, paid, unpaid, pastdue])
client: client=23445
-}
getAllInvoices : String -> String -> Dict String String -> Request (List Invoice)
getAllInvoices accountId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrl accountId token params
        , body = emptyBody
        , expect = expectJson invoicesDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Show A Single Invoice

GET https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}

HTTP Response: 200 OK
-}
getInvoice : String -> Int -> String -> Request Invoice
getInvoice accountId invoiceId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson invoiceDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Delete Existing Invoice

DELETE https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}

HTTP Response: 200 OK
-}
deleteInvoice : String -> Int -> String -> Request String
deleteInvoice accountId invoiceId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Update Existing Invoice

PUT https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}

HTTP Response: 200 OK, in addition to LOCATION: /invoices/{INVOICEID}
-}
updateInvoice : String -> Invoice -> String -> Request String
updateInvoice accountId invoice token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoice.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeInvoice invoice
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Create An Invoice

POST https://YOURACCOUNT.harvestapp.com/invoices

HTTP Response: 201 Created
-}
createInvoice : String -> String -> Invoice -> Request String
createInvoice accountId token invoice =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices?access_token=" ++ token
        , body = jsonBody <| encodeInvoice invoice
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Show Invoice Messages

GET https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/messages
-}
getMessagesForInvoice : String -> String -> Int -> Request (List Message)
getMessagesForInvoice accountId token invoiceId =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "/messages?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson messagesDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Show a particular invoice message:

GET https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/message/{MESSAGEID}
-}
getMessageForInvoice : String -> String -> Int -> Int -> Request Message
getMessageForInvoice accountId token invoiceId messageId =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "/messages" ++ (toString messageId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson messageDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Send An Invoice

POST https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/messages

HTTP Response: 201 Created
-}
sendInvoice : String -> String -> SimpleMessage -> Request String
sendInvoice accountId token message =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString message.invoiceId) ++ "/messages?access_token=" ++ token
        , body = jsonBody <| encodeSimpleMessage message
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Delete Existing Message

DELETE https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/messages/{MESSAGEID}
-}
deleteMessage : String -> Int -> Int -> String -> Request String
deleteMessage accountId invoiceId messageId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "/messages/" ++ (toString messageId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Mark An Invoice As Sent

POST https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/messages/mark_as_sent
-}
markInvoiceAsSent : String -> Int -> String -> Request String
markInvoiceAsSent accountId invoiceId token =
    createRequestForMark accountId invoiceId token "mark_as_sent"


{-|
Change A Sent Invoice To Draft

POST https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/messages/mark_as_draft
-}
markInvoiceAsDraft : String -> Int -> String -> Request String
markInvoiceAsDraft accountId invoiceId token =
    createRequestForMark accountId invoiceId token "mark_as_draft"


{-|
Write An Invoice Off

POST https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/messages/mark_as_closed
-}
markInvoiceAsClosed : String -> Int -> String -> Request String
markInvoiceAsClosed accountId invoiceId token =
    createRequestForMark accountId invoiceId token "mark_as_closed"


{-|
Re-open An Invoice

POST https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/messages/re_open
-}
markInvoiceAsOpen : String -> Int -> String -> Request String
markInvoiceAsOpen accountId invoiceId token =
    createRequestForMark accountId invoiceId token "re_open"


{-|
Show All Categories

GET https://YOURACCOUNT.harvestapp.com/invoice_item_categories
-}
getInvoiceCategories : String -> String -> Request (List InvoiceCategory)
getInvoiceCategories accountId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoice_item_categories?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson invoiceCategoriesDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Create New Category

POST https://YOURACCOUNT.harvestapp.com/invoice_item_categories
-}
createInvoiceCategory : String -> String -> InvoiceCategory -> Request String
createInvoiceCategory accountId token invoiceCategory =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoice_item_categories?access_token=" ++ token
        , body = jsonBody <| encodeInvoiceCategory invoiceCategory
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Update Existing Category

PUT https://YOURACCOUNT.harvestapp.com/invoice_item_categories/{CATEGORYID}
-}
updateInvoiceCategory : String -> String -> InvoiceCategory -> Request String
updateInvoiceCategory accountId token invoiceCategory =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoice_item_categories/" ++ (toString invoiceCategory.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeInvoiceCategory invoiceCategory
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Delete A Category

DELETE https://YOURACCOUNT.harvestapp.com/invoice_item_categories/{CATEGORYID}
-}
deleteInvoiceCategory : String -> Int -> String -> Request String
deleteInvoiceCategory accountId invoiceCategoryId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoice_item_categories/" ++ (toString invoiceCategoryId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Show Payments For An Invoice

GET https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/payments
-}
getPaymentsForInvoice : String -> Int -> String -> Request (List Payment)
getPaymentsForInvoice accountId invoiceId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson paymentsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Return a single, specific payment:

GET https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/payments/{PAYMENTID}
-}
getPaymentForInvoice : String -> Int -> Int -> String -> Request Payment
getPaymentForInvoice accountId invoiceId paymentId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "/payments/" ++ (toString paymentId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson paymentDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Delete Existing Payment

DELETE https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/payments{PAYMENTID}
-}
deletePayment : String -> Int -> Int -> String -> Request String
deletePayment accountId invoiceId paymentId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "/payments" ++ (toString paymentId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


{-|
Create A New Payment

POST https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}/payments

The fields paid-at and amount are required, and optionally notes can be included.
-}
createPayment : String -> String -> Payment -> Request String
createPayment accountId token payment =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString payment.invoiceId) ++ "/payments?access_token=" ++ token
        , body = jsonBody <| encodePayment payment
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Helpers -}


encodeSimpleMessage : SimpleMessage -> JE.Value
encodeSimpleMessage c =
    JE.object
        [ ( "message"
          , JE.object
                [ ( "body", JE.string c.body )
                , ( "recipients", JE.string c.recipients )
                , ( "attach_pdf", JE.bool c.attachPdf )
                , ( "send_me_an_email", JE.bool c.sendMeAnEmail )
                , ( "include_pay_pal_link", JE.bool c.includePayPalLink )
                ]
          )
        ]


encodePayment : Payment -> JE.Value
encodePayment c =
    JE.object
        [ ( "payment"
          , JE.object
                [ ( "paid_at", JE.string (toUtcIsoString c.paidAt) )
                , ( "amount", JE.float c.amount )
                , ( "notes", JEE.maybe JE.string c.notes )
                ]
          )
        ]


encodeInvoiceCategory : InvoiceCategory -> JE.Value
encodeInvoiceCategory c =
    JE.object
        [ ( "invoice_item_category"
          , JE.object
                [ ( "name", JE.string c.name )
                ]
          )
        ]


createRequestForMark : String -> Int -> String -> String -> Request String
createRequestForMark accountId invoiceId token invoiveType =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "/messages/" ++ invoiveType ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


encodeInvoice : Invoice -> JE.Value
encodeInvoice c =
    JE.object
        [ ( "invoice"
          , JE.object
                [ ( "due_at_human_format", JE.string c.dueAtHumanFormat )
                , ( "currency", JE.string c.currency )
                , ( "discount", JEE.maybe JE.string c.discount )
                , ( "discount_amount", JEE.maybe JE.float c.discountAmount )
                , ( "issued_at", JE.string (toFormattedString "yyyy-MM-dd" c.issuedAt) )
                , ( "subject", JEE.maybe JE.string c.subject )
                , ( "notes", JEE.maybe JE.string c.notes )
                , ( "number", JEE.maybe JE.int c.number )
                , ( "kind", JEE.maybe JE.string c.kind )
                , ( "projects_to_invoice", JEE.maybe JE.string c.kind )
                , ( "period-start", JE.string (toFormattedString "yyyy-MM-dd" c.periodStart) )
                , ( "period_end", JE.string (toFormattedString "yyyy-MM-dd" c.periodEnd) )
                , ( "purchase_order", JE.string c.purchaseOrder )
                , ( "tax", JE.float c.tax )
                , ( "tax2", JEE.maybe JE.float c.tax2 )
                ]
          )
        ]



{-
   Belongs to invoice, but Harvest API docs are not really clear. Won't support for now.

    import-hours	Hours to import into invoices. Options: all(import all hours), yes (import hours using period-start, period-end), no (do not import hours).
    import-expenses	Expenses to import into invoices. Options: all(import all expenses), yes (import expenses using expense-period-start, expense-period-end), no (do not import expenses).
    expense-summary-type	Summary type for expenses in an invoice. Options: project, people, category, detailed.
    expense-period-start	Date for included project expenses. (Example: 2015-04-22)
    expense-period-end	End date for included project expenses. (Example: 2015-05-22)
    csv-line-items	Used to create line items in free-form invoices. Entries should have their entries enclosed in quotes when they contain extra commas. This is especially important if you are using a number format which uses commas as the decimal separator.
-}


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/invoices?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
