module Monzo exposing (Account, Record(..), parseCsv, recordsToCrunchCsv, timeOfRecord)

import Array
import Calendar
import Clock
import Csv
import DateTime
import Iso8601
import Parser exposing ((|.), (|=), Parser)
import Time exposing (Month(..))


type alias Account =
    { records : List Record
    , totalAmountInPence : Int
    }


parseCsv : String -> Result String Account
parseCsv str =
    str
        |> Csv.parse makeRecordParser
        |> Result.mapError
            (\deadends ->
                {- TODO -}
                "Error parsing CSV"
            )
        |> Result.map
            (\records ->
                { records = {- Debug.log "rows" <| -} calculateRunningTotalAmounts records
                , totalAmountInPence = calculateTotalAmount records
                }
            )


calculateRunningTotalAmounts : List Record -> List Record
calculateRunningTotalAmounts records =
    let
        go : Int -> List Record -> List Record
        go runningTotal recordsToProcess =
            case recordsToProcess of
                [] ->
                    []

                record :: tail ->
                    case record of
                        TransactionRecord transaction ->
                            let
                                newTotal =
                                    runningTotal + transaction.amountInPence
                            in
                            TransactionRecord { transaction | runningTotalAmountInPence = newTotal }
                                :: go newTotal tail

                        PotTransferRecord potTransferRecord ->
                            PotTransferRecord { potTransferRecord | runningTotalAmountInPence = runningTotal } :: go runningTotal tail

                        ActiveCardCheckRecord activeCardCheckRecord ->
                            ActiveCardCheckRecord { activeCardCheckRecord | runningTotalAmountInPence = runningTotal }
                                :: go runningTotal tail
    in
    go 0 records


calculateTotalAmount : List Record -> Int
calculateTotalAmount records =
    let
        calculateTotalAmountHelp : Record -> Int -> Int
        calculateTotalAmountHelp record total =
            case record of
                TransactionRecord transaction ->
                    total + transaction.amountInPence

                PotTransferRecord _ ->
                    total

                ActiveCardCheckRecord _ ->
                    total
    in
    List.foldl calculateTotalAmountHelp 0 records


amount : String -> Parser Int
amount str =
    let
        parser =
            Parser.succeed (\pounds pence -> String.toInt (pounds ++ pence))
                |= Parser.getChompedString
                    (Parser.succeed ()
                        |. Parser.oneOf [ Parser.chompIf (\c -> c == '-'), Parser.succeed () ]
                        |. Parser.chompIf Char.isDigit
                        |. Parser.chompWhile Char.isDigit
                    )
                |. Parser.symbol "."
                |= Parser.getChompedString
                    (Parser.succeed ()
                        |. Parser.chompIf Char.isDigit
                        |. Parser.chompIf Char.isDigit
                    )
                |. Parser.end
    in
    case Parser.run parser str of
        Ok (Just n) ->
            Parser.succeed n

        _ ->
            Parser.problem <| "Unable to parse " ++ str ++ " as a monetary value"


zeroAmount : String -> Parser ()
zeroAmount str =
    if str == "0.00" then
        Parser.succeed ()

    else
        Parser.problem <| "Expected " ++ str ++ " to be 0.00 for an active card check"


exactString : String -> String -> Parser ()
exactString str input =
    if input == str then
        Parser.succeed ()

    else
        Parser.problem <| "Expected " ++ input ++ " to be ‘" ++ str ++ "’"



-- potName : String -> Parser String
-- potName str =
--     let
--         parser =
--             Parser.succeed identity
--                 |. Parser.oneOf [ Parser.symbol "Transfer to pot ", Parser.symbol "Transfer from pot " ]
--                 |= (Parser.getChompedString <| Parser.chompWhile (always True))
--                 |. Parser.end
--     in
--     case Parser.run parser str of
--         Ok s ->
--             Parser.succeed s
--         _ ->
--             Parser.problem <| "Unable to parse " ++ str ++ " as a pot name"


type Record
    = TransactionRecord Transaction
    | PotTransferRecord PotTransfer
    | ActiveCardCheckRecord ActiveCardCheck


timeOfRecord : Record -> Time.Posix
timeOfRecord record =
    case record of
        TransactionRecord { time } ->
            time

        PotTransferRecord { time } ->
            time

        ActiveCardCheckRecord { time } ->
            time


type alias Transaction =
    { time : Time.Posix
    , runningTotalAmountInPence : Int
    , amountInPence : Int
    , description : String
    }


makeTransaction : Calendar.Date -> Clock.Time -> String -> Int -> String -> String -> Transaction
makeTransaction date time name amountInPence notes description =
    let
        combinedDescription =
            [ name, description, notes ] |> List.filter (not << String.isEmpty) |> String.join " - "
    in
    { time = DateTime.fromDateAndTime date time |> DateTime.toPosix, runningTotalAmountInPence = 0, amountInPence = amountInPence, description = combinedDescription }


type alias PotTransfer =
    { time : Time.Posix
    , runningTotalAmountInPence : Int
    , amountInPence : Int
    , potName : String
    }


makePotTransfer : Calendar.Date -> Clock.Time -> String -> Int -> PotTransfer
makePotTransfer date time potName amountInPence =
    { time = DateTime.fromDateAndTime date time |> DateTime.toPosix, runningTotalAmountInPence = 0, amountInPence = amountInPence, potName = potName }


type alias ActiveCardCheck =
    { time : Time.Posix
    , runningTotalAmountInPence : Int
    , description : String
    }


makeActiveCardCheck : Calendar.Date -> Clock.Time -> String -> String -> ActiveCardCheck
makeActiveCardCheck date time name description =
    let
        combinedDescription =
            [ name, description ] |> List.filter (not << String.isEmpty) |> String.join " - "
    in
    { time = DateTime.fromDateAndTime date time |> DateTime.toPosix, runningTotalAmountInPence = 0, description = combinedDescription }



-- iso8601 : String -> Parser Time.Posix
-- iso8601 str =
--     case Iso8601.toTime str of
--         Err _ ->
--             Parser.problem <| "Unable to parse " ++ str ++ " as a date/time"
--         Ok time ->
--             Parser.succeed time


zeroPaddedInt : Parser Int
zeroPaddedInt =
    let
        toInt s =
            String.toInt s |> Maybe.map Parser.succeed |> Maybe.withDefault (Parser.problem "Unable to parse as int")
    in
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen toInt


ukDate : String -> Parser Calendar.Date
ukDate str =
    let
        lookupMonth : Int -> Parser Month
        lookupMonth oneIndexedMonth =
            Array.get (oneIndexedMonth - 1) Calendar.months
                |> Maybe.map Parser.succeed
                |> Maybe.withDefault (Parser.problem ("Failed to parse " ++ String.fromInt oneIndexedMonth ++ " as a Month"))

        rawDate : Parser Calendar.RawDate
        rawDate =
            Parser.succeed (\d m y -> Calendar.RawDate y m d)
                |= zeroPaddedInt
                |. Parser.symbol "/"
                |= (zeroPaddedInt |> Parser.andThen lookupMonth)
                |. Parser.symbol "/"
                |= zeroPaddedInt
                |. Parser.end

        parser =
            rawDate |> Parser.map Calendar.fromRawParts
    in
    case Parser.run parser str of
        Ok (Just d) ->
            Parser.succeed d

        _ ->
            Parser.problem <| "Unable to parse " ++ str ++ " as a UK dd/mm/yyyy date"


ukTime : String -> Parser Clock.Time
ukTime str =
    let
        rawTime : Parser Clock.RawTime
        rawTime =
            Parser.succeed (\h m s -> Clock.RawTime h m s 0)
                |= zeroPaddedInt
                |. Parser.symbol ":"
                |= zeroPaddedInt
                |. Parser.symbol ":"
                |= zeroPaddedInt
                |. Parser.end

        parser =
            rawTime |> Parser.map Clock.fromRawParts
    in
    case Parser.run parser str of
        Ok (Just t) ->
            Parser.succeed t

        Err error ->
            Parser.problem (Parser.deadEndsToString error)

        _ ->
            Parser.problem <| "Unable to parse " ++ str ++ " as a UK hh:mm:ss time"


makeRecordParser : Csv.MakeRecordParser Record
makeRecordParser helpers =
    Parser.oneOf
        [ Parser.backtrackable <| Parser.map ActiveCardCheckRecord <| makeActiveCardCheckParser helpers
        , Parser.backtrackable <| Parser.map PotTransferRecord <| makePotTransferParser helpers
        , Parser.map TransactionRecord <| makeTransactionParser helpers
        ]


makeTransactionParser : Csv.MakeRecordParser Transaction
makeTransactionParser { field, fieldSep, lineSep } =
    Parser.succeed makeTransaction
        -- Transaction ID
        |. field
        |. fieldSep
        -- Date
        |= Parser.andThen ukDate field
        |. fieldSep
        -- Time
        |= Parser.andThen ukTime field
        |. fieldSep
        -- Type
        |. field
        |. fieldSep
        -- Name
        |= field
        |. fieldSep
        -- Emoji
        |. field
        |. fieldSep
        -- Category
        |. field
        |. fieldSep
        -- Amount
        |= Parser.andThen amount field
        |. fieldSep
        -- Currency
        |. field
        |. fieldSep
        -- Local amount
        |. field
        |. fieldSep
        -- Local currency
        |. field
        |. fieldSep
        -- Notes and #tags
        |= field
        |. fieldSep
        -- Address
        |. field
        |. fieldSep
        -- Receipt
        |. field
        |. fieldSep
        -- Description
        |= field
        |. fieldSep
        -- Category split
        |. field
        |. lineSep


makePotTransferParser : Csv.MakeRecordParser PotTransfer
makePotTransferParser { field, fieldSep, lineSep } =
    Parser.succeed makePotTransfer
        -- Transaction ID
        |. field
        |. fieldSep
        -- Date
        |= Parser.andThen ukDate field
        |. fieldSep
        -- Time
        |= Parser.andThen ukTime field
        |. fieldSep
        -- Type
        |. Parser.andThen (exactString "Pot transfer") field
        |. fieldSep
        -- Name
        |= field
        |. fieldSep
        -- Emoji
        |. field
        |. fieldSep
        -- Category
        |. field
        |. fieldSep
        -- Amount
        |= Parser.andThen amount field
        |. fieldSep
        -- Currency
        |. field
        |. fieldSep
        -- Local amount
        |. field
        |. fieldSep
        -- Local currency
        |. field
        |. fieldSep
        -- Notes and #tags
        |. field
        |. fieldSep
        -- Address
        |. field
        |. fieldSep
        -- Receipt
        |. field
        |. fieldSep
        -- Description
        |. field
        |. fieldSep
        -- Category split
        |. field
        |. lineSep


makeActiveCardCheckParser : Csv.MakeRecordParser ActiveCardCheck
makeActiveCardCheckParser { field, fieldSep, lineSep } =
    Parser.succeed makeActiveCardCheck
        -- Transaction ID
        |. field
        |. fieldSep
        -- Date
        |= Parser.andThen ukDate field
        |. fieldSep
        -- Time
        |= Parser.andThen ukTime field
        |. fieldSep
        -- Type
        |. field
        |. fieldSep
        -- Name
        |= field
        |. fieldSep
        -- Emoji
        |. field
        |. fieldSep
        -- Category
        |. field
        |. fieldSep
        -- Amount
        |. Parser.andThen zeroAmount field
        |. fieldSep
        -- Currency
        |. field
        |. fieldSep
        -- Local amount
        |. field
        |. fieldSep
        -- Local currency
        |. field
        |. fieldSep
        -- Notes and #tags
        |. Parser.andThen (exactString "Active card check") field
        |. fieldSep
        -- Address
        |. field
        |. fieldSep
        -- Receipt
        |. field
        |. fieldSep
        -- Description
        |= field
        |. fieldSep
        -- Category split
        |. field
        |. lineSep


transactionToCrunchCsvRow : Transaction -> String
transactionToCrunchCsvRow transaction =
    let
        date =
            Calendar.fromPosix transaction.time

        dateField =
            String.join " "
                [ date |> Calendar.getDay |> String.fromInt
                , date |> Calendar.getMonth |> monthToCrunchStr
                , date |> Calendar.getYear |> String.fromInt
                ]

        descriptionField =
            "\"" ++ String.replace "\"" "\"\"" transaction.description ++ "\""

        formatAmount amountInPence =
            let
                absAmountInPence =
                    abs amountInPence

                negative =
                    amountInPence < 0

                pounds =
                    absAmountInPence // 100

                pence =
                    absAmountInPence - (pounds * 100)
            in
            (if negative then
                "-"

             else
                ""
            )
                ++ String.fromInt pounds
                ++ "."
                ++ String.padLeft 2 '0' (String.fromInt pence)

        amountField =
            formatAmount transaction.amountInPence

        balanceField =
            formatAmount transaction.runningTotalAmountInPence
    in
    String.join "," [ dateField, descriptionField, amountField, balanceField ]


monthToCrunchStr : Month -> String
monthToCrunchStr month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


recordToCrunchCsvRow : Record -> Maybe String
recordToCrunchCsvRow record =
    case record of
        TransactionRecord transaction ->
            Just <| transactionToCrunchCsvRow transaction

        PotTransferRecord _ ->
            Nothing

        ActiveCardCheckRecord _ ->
            Nothing


recordsToCrunchCsv : List Record -> String
recordsToCrunchCsv records =
    let
        header =
            [ "date", "description", "amount", "balance" ]
                |> String.join ","

        body =
            records |> List.filterMap recordToCrunchCsvRow
    in
    (header :: body) |> String.join "\n"
