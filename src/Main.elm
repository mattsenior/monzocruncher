module Main exposing (main)

import Browser
import Calendar
import File exposing (File)
import File.Download
import File.Select
import Html as Html exposing (Html)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events
import Iso8601
import Json.Decode as Decode
import List.Extra
import Monzo
import Pivot exposing (Pivot)
import Result.Extra
import Task
import Time exposing (Month(..))


classes : List String -> Html.Attribute Msg
classes =
    String.join " " >> class


type alias Account =
    { fileName : String
    , months : Pivot AccountMonth
    , totalAmountInPence : Int
    , totalConfirmation : Confirmation
    , page : AccountPage
    }


type AccountPage
    = AccountListPage
    | AccountMonthPage


type alias AccountMonth =
    { year : Int
    , month : Month
    , records : List Monzo.Record
    }


type Confirmation
    = Pending
    | Confirmed
    | Rejected


type Model
    = AccountLoaded Account
    | Dropzone { isDraggingOver : Bool }


initialModel : Model
initialModel =
    Dropzone { isDraggingOver = False }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = Reset
    | DragEnter
    | DragLeave
    | OpenFilePicker
    | ConfirmTotal
    | RejectTotal
    | FileDropped File
    | AccountParsed String Monzo.Account Time.Posix
    | FileError FileError
    | ViewAccountMonthPage { year : Int, month : Month }
    | ViewAccountListPage
    | ViewPrevMonth
    | ViewNextMonth
    | DownloadTransactions


type FileError
    = CsvParseError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialModel, Cmd.none )

        DragEnter ->
            case model of
                AccountLoaded _ ->
                    ( model, Cmd.none )

                Dropzone subModel ->
                    ( Dropzone { subModel | isDraggingOver = True }, Cmd.none )

        DragLeave ->
            case model of
                AccountLoaded _ ->
                    ( model, Cmd.none )

                Dropzone subModel ->
                    ( Dropzone { subModel | isDraggingOver = False }, Cmd.none )

        OpenFilePicker ->
            ( model, File.Select.file [ "text/csv" ] FileDropped )

        ConfirmTotal ->
            case model of
                Dropzone _ ->
                    ( model, Cmd.none )

                AccountLoaded account ->
                    ( AccountLoaded { account | totalConfirmation = Confirmed }, Cmd.none )

        RejectTotal ->
            case model of
                Dropzone _ ->
                    ( model, Cmd.none )

                AccountLoaded account ->
                    ( AccountLoaded { account | totalConfirmation = Rejected }, Cmd.none )

        FileDropped file ->
            let
                resultToMsg result =
                    case result of
                        Ok ( account, now ) ->
                            AccountParsed (File.name file) account now

                        Err err ->
                            FileError err

                parseFileTask =
                    file
                        |> File.toString
                        |> Task.andThen
                            (Monzo.parseCsv
                                >> Result.mapError CsvParseError
                                >> Result.Extra.toTask
                            )

                cmd =
                    Task.map2 Tuple.pair parseFileTask Time.now
                        |> Task.attempt resultToMsg
            in
            ( Dropzone { isDraggingOver = False }, cmd )

        AccountParsed fileName account now ->
            let
                firstMonth : Calendar.Date
                firstMonth =
                    List.head account.records
                        |> Maybe.map (Monzo.timeOfRecord >> Calendar.fromPosix)
                        |> Maybe.withDefault (Calendar.fromPosix now)

                nowDate =
                    Calendar.fromPosix now

                isSameMonth : Calendar.Date -> Calendar.Date -> Bool
                isSameMonth a b =
                    Calendar.getYear a == Calendar.getYear b && Calendar.getMonth a == Calendar.getMonth b

                isRecordInMonth : Calendar.Date -> Monzo.Record -> Bool
                isRecordInMonth month record =
                    isSameMonth month (record |> Monzo.timeOfRecord |> Calendar.fromPosix)

                doRecords : List Monzo.Record -> Calendar.Date -> Pivot AccountMonth -> Pivot AccountMonth
                doRecords records month output =
                    let
                        ( recordsThisMonth, restRecords ) =
                            List.Extra.span (isRecordInMonth month) records

                        newOutput =
                            Pivot.appendGoL
                                { year = Calendar.getYear month
                                , month = Calendar.getMonth month
                                , records = recordsThisMonth
                                }
                                output

                        isFutureYear =
                            Calendar.getYear month > Calendar.getYear nowDate

                        isSameYear =
                            Calendar.getYear month == Calendar.getYear nowDate

                        isSameOrLaterMonth =
                            (Calendar.getMonth month |> Calendar.monthToInt) >= (Calendar.getMonth nowDate |> Calendar.monthToInt)
                    in
                    if List.isEmpty restRecords && (isFutureYear || isSameYear && isSameOrLaterMonth) then
                        newOutput

                    else
                        doRecords restRecords (Calendar.incrementMonth month) newOutput

                ( firstMonthRecords, remainingMonthRecords ) =
                    List.Extra.span (isRecordInMonth firstMonth) account.records

                months =
                    doRecords remainingMonthRecords
                        (Calendar.incrementMonth firstMonth)
                        (Pivot.singleton
                            { year = Calendar.getYear firstMonth
                            , month = Calendar.getMonth firstMonth
                            , records = firstMonthRecords
                            }
                        )
            in
            ( AccountLoaded
                { fileName = fileName
                , months = months
                , totalAmountInPence = account.totalAmountInPence
                , totalConfirmation = Pending
                , page = AccountListPage
                }
            , Cmd.none
            )

        ViewAccountListPage ->
            case model of
                Dropzone _ ->
                    ( model, Cmd.none )

                AccountLoaded account ->
                    ( AccountLoaded { account | page = AccountListPage }, Cmd.none )

        ViewAccountMonthPage { month, year } ->
            case model of
                Dropzone _ ->
                    ( model, Cmd.none )

                AccountLoaded account ->
                    let
                        newMonths =
                            Pivot.withRollback
                                (Pivot.firstWith (\m -> m.year == year && m.month == month))
                                account.months
                    in
                    ( AccountLoaded
                        { account
                            | months = newMonths
                            , page = AccountMonthPage
                        }
                    , Cmd.none
                    )

        ViewPrevMonth ->
            case model of
                Dropzone _ ->
                    ( model, Cmd.none )

                AccountLoaded account ->
                    ( AccountLoaded { account | months = Pivot.withRollback Pivot.goL account.months }, Cmd.none )

        ViewNextMonth ->
            case model of
                Dropzone _ ->
                    ( model, Cmd.none )

                AccountLoaded account ->
                    ( AccountLoaded { account | months = Pivot.withRollback Pivot.goR account.months }, Cmd.none )

        FileError _ ->
            ( model, Cmd.none )

        DownloadTransactions ->
            case model of
                Dropzone _ ->
                    ( model, Cmd.none )

                AccountLoaded account ->
                    let
                        { year, month, records } =
                            Pivot.getC account.months

                        fileName =
                            String.fromInt year ++ "-" ++ (month |> Calendar.monthToInt |> String.fromInt |> String.padLeft 2 '0') ++ "-transactions.csv"

                        download =
                            File.Download.string fileName "text/csv" (Monzo.recordsToCrunchCsv records)
                    in
                    ( model, download )


fileDecoder : Decode.Decoder ( Msg, Bool )
fileDecoder =
    Decode.field "dataTransfer" (Decode.field "files" (Decode.index 0 File.decoder))
        |> Decode.map (\file -> ( FileDropped file, True ))


view : Model -> Html Msg
view model =
    case model of
        AccountLoaded account ->
            viewAccount account

        Dropzone { isDraggingOver } ->
            viewDropzone isDraggingOver


viewDropzone : Bool -> Html Msg
viewDropzone isDraggingOver =
    Html.main_ []
        [ viewHeader
        , Html.div
            [ class "container mx-auto px-6" ]
            [ Html.a
                [ Events.preventDefaultOn "dragover" (Decode.succeed ( DragEnter, True ))
                , Events.on "dragleave" (Decode.succeed DragLeave)
                , Events.preventDefaultOn "drop" fileDecoder
                , Events.onClick OpenFilePicker
                , classes
                    [ "bg-gray-800 border-8 border-gray-900"
                    , "hover:bg-gray-700"
                    , "active:bg-gray-800"
                    , "cursor-pointer h-64 text-white flex items-center justify-center text-center"
                    ]
                , Attr.classList [ ( "bg-pink-800", isDraggingOver ) ]
                ]
                [ Html.span []
                    [ Html.text "Upload your Monzo business CSV" ]
                ]
            ]
        ]


viewHeader : Html Msg
viewHeader =
    Html.h1 [ class "container mx-auto font-display font-bold text-4xl text-white mt-4 mb-6 px-6" ]
        [ Html.text "monzocruncher" ]


viewAccount : Account -> Html Msg
viewAccount account =
    case account.totalConfirmation of
        Pending ->
            Html.main_ []
                [ viewHeader
                , Html.div [ class "container mx-auto px-6 text-lg" ]
                    [ Html.p []
                        [ Html.text "Is your total bank balance (including pots) exactly "
                        , Html.span [ class "font-bold" ] [ Html.text <| viewAmount account.totalAmountInPence ]
                        ]
                    , Html.button
                        [ Events.onClick ConfirmTotal
                        , classes
                            [ "block my-4 py-2 w-full rounded-full text-xl text-center"
                            , "bg-white hover:bg-teal-400"
                            ]
                        ]
                        [ Html.text "Yessir" ]
                    , Html.button
                        [ Events.onClick RejectTotal
                        , classes
                            [ "block my-4 py-2 w-full rounded-full text-center"
                            , "border-2 border-red-700 hover:bg-red-700"
                            ]
                        ]
                        [ Html.text "No, no it’s not" ]
                    ]
                ]

        Confirmed ->
            viewAccountPage account

        Rejected ->
            Html.div []
                [ Html.text "Oh no!"
                , Html.button [ Events.onClick Reset ] [ Html.text "Start again" ]
                ]


viewAmount : Int -> String
viewAmount amountInPence =
    let
        isNegative =
            amountInPence < 0

        absAmountInPence =
            abs amountInPence

        pounds =
            absAmountInPence // 100

        pence =
            absAmountInPence - (pounds * 100)

        poundsString =
            let
                poundsChars =
                    pounds |> String.fromInt |> String.toList |> List.reverse

                addSeparators chrs =
                    case chrs of
                        a :: b :: c :: d :: tail ->
                            a :: b :: c :: ',' :: addSeparators (d :: tail)

                        _ ->
                            chrs
            in
            addSeparators poundsChars |> List.reverse |> String.fromList
    in
    (if isNegative then
        "-"

     else
        ""
    )
        ++ "£"
        ++ poundsString
        ++ "."
        ++ String.padLeft 2 '0' (String.fromInt pence)


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


viewAccountPage : Account -> Html Msg
viewAccountPage account =
    case account.page of
        AccountListPage ->
            viewMonthList account

        AccountMonthPage ->
            viewMonth account


viewMonth : Account -> Html Msg
viewMonth account =
    let
        { month, year, records } =
            Pivot.getC account.months

        nextMonth =
            Pivot.goL account.months |> Maybe.map (Pivot.getC >> (\m -> monthToString m.month ++ " " ++ String.fromInt m.year))

        prevMonth =
            Pivot.goR account.months |> Maybe.map (Pivot.getC >> (\m -> monthToString m.month ++ " " ++ String.fromInt m.year))

        navs =
            List.concat
                [ nextMonth |> Maybe.map (\m -> [ Html.a [ Events.onClick ViewPrevMonth, class "cursor-pointer" ] [ Html.text m ] ]) |> Maybe.withDefault []
                , prevMonth |> Maybe.map (\m -> [ Html.a [ Events.onClick ViewNextMonth, class "cursor-pointer" ] [ Html.text m ] ]) |> Maybe.withDefault []
                ]

        viewRecord record =
            case record of
                Monzo.TransactionRecord transaction ->
                    Html.tr []
                        [ Html.td [] [ transaction.time |> Iso8601.fromTime |> Html.text ]
                        , Html.td
                            []
                            [ Html.text "Transaction" ]
                        ]

                Monzo.PotTransferRecord potTransfer ->
                    Html.tr [] [ Html.td [] [ Html.text "Pot Transfer" ] ]

                Monzo.ActiveCardCheckRecord activeCardCheck ->
                    Html.tr [] [ Html.td [] [ Html.text "Active card check" ] ]
    in
    Html.main_ []
        [ viewHeader
        , Html.div [ class "container mx-auto px-6 text-lg" ]
            [ Html.nav [ class "flex justify-between" ] navs
            , Html.h2 [ class "text-center font-semibold text-2xl whitespace-no-wrap" ]
                [ Html.text <| monthToString month ++ " " ++ String.fromInt year
                ]
            , Html.button [ Events.onClick DownloadTransactions ] [ Html.text "Download transactions" ]
            , Html.table [ class "" ]
                (List.map viewRecord records)
            ]
        ]


viewMonthList : Account -> Html Msg
viewMonthList account =
    let
        viewStatementSummary : AccountMonth -> Html Msg
        viewStatementSummary { month, year, records } =
            let
                numRecords =
                    List.length records

                numTransactions =
                    records
                        |> List.filter
                            (\record ->
                                case record of
                                    Monzo.TransactionRecord _ ->
                                        True

                                    _ ->
                                        False
                            )
                        |> List.length

                numOther =
                    numRecords - numTransactions

                pluralise s n =
                    s
                        ++ (if n == 1 then
                                ""

                            else
                                "s"
                           )
            in
            Html.li [ class "my-2" ]
                [ Html.a
                    [ class "bg-white p-4 flex justify-between cursor-pointer"
                    , Events.onClick (ViewAccountMonthPage { year = year, month = month })
                    ]
                    [ Html.span [] [ Html.text <| monthToString month ++ " " ++ String.fromInt year ]
                    , Html.span []
                        [ Html.span [ class "text-teal-400" ]
                            [ Html.text <|
                                String.fromInt numTransactions
                                    ++ " "
                                    ++ pluralise "transaction" numTransactions
                            ]
                        , Html.br [] []
                        , Html.span []
                            [ Html.text <|
                                String.fromInt numOther
                                    ++ " "
                                    ++ pluralise "Other record" numOther
                            ]
                        ]
                    ]
                ]
    in
    Html.main_ []
        [ viewHeader
        , Html.ol [ class "container mx-auto px-6 text-lg" ]
            (Pivot.toList
                account.months
                |> List.map viewStatementSummary
            )
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
