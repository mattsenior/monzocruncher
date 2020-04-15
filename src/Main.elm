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
import Svg
import Svg.Attributes as SvgAttr
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
    | ParseFailure


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
    | DownloadTransactions AccountMonth


type FileError
    = CsvParseError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, Reset ) ->
            ( initialModel, Cmd.none )

        ( Dropzone subModel, DragEnter ) ->
            ( Dropzone { subModel | isDraggingOver = True }, Cmd.none )

        ( Dropzone subModel, DragLeave ) ->
            ( Dropzone { subModel | isDraggingOver = False }, Cmd.none )

        ( _, OpenFilePicker ) ->
            ( model, File.Select.file [ "text/csv" ] FileDropped )

        ( AccountLoaded account, ConfirmTotal ) ->
            ( AccountLoaded { account | totalConfirmation = Confirmed }, Cmd.none )

        ( AccountLoaded account, RejectTotal ) ->
            ( AccountLoaded { account | totalConfirmation = Rejected }, Cmd.none )

        ( _, FileDropped file ) ->
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

        ( _, AccountParsed fileName account now ) ->
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

        ( AccountLoaded account, ViewAccountListPage ) ->
            ( AccountLoaded { account | page = AccountListPage }, Cmd.none )

        ( AccountLoaded account, ViewAccountMonthPage { month, year } ) ->
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

        ( AccountLoaded account, ViewPrevMonth ) ->
            ( AccountLoaded { account | months = Pivot.withRollback Pivot.goL account.months }, Cmd.none )

        ( AccountLoaded account, ViewNextMonth ) ->
            ( AccountLoaded { account | months = Pivot.withRollback Pivot.goR account.months }, Cmd.none )

        ( _, FileError _ ) ->
            ( ParseFailure, Cmd.none )

        ( AccountLoaded account, DownloadTransactions accountMonth ) ->
            let
                { year, month, records } =
                    accountMonth

                fileName =
                    String.fromInt year ++ "-" ++ (month |> Calendar.monthToInt |> String.fromInt |> String.padLeft 2 '0') ++ "-transactions.csv"

                download =
                    File.Download.string fileName "text/csv" (Monzo.recordsToCrunchCsv records)
            in
            ( model, download )

        _ ->
            ( model, Cmd.none )


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

        ParseFailure ->
            viewParseFailure


viewParseFailure : Html Msg
viewParseFailure =
    viewLayout
        [ Html.div [ class "container mx-auto px-6" ]
            [ Html.p []
                [ Html.span [ class "font-bold" ] [ Html.text "Oh no!" ]
                , Html.text " Something went wrong when trying to parse the file you selected. Perhaps Monzo has changed the format of their CSV exports, or perhaps there’s a bug with this tool."
                ]
            , Html.p [ class "mt-8" ]
                [ Html.text "If you would like to report this as a bug, please "
                , Html.a [ Attr.href "https://github.com/mattsenior/monzocruncher/issues", class "font-bold underline hover:text-gray-900" ] [ Html.text "open an issue on the GitHub repository" ]
                , Html.text " and we can chat about it."
                ]
            , Html.button [ Events.onClick Reset, class "mt-8 font-bold underline hover:text-gray-900" ] [ Html.text "Start again" ]
            ]
        ]


viewPreStep1 : Html Msg
viewPreStep1 =
    Html.p []
        [ Html.text "Convert your Monzo Business CSV exports into a CSV format that Crunch will recognise for bank reconciliation. "
        , Html.br [] []
        , Html.a [ Attr.href "#notes", class "font-bold underline hover:text-gray-900" ] [ Html.text "Read the footer blurb for caveats, etc." ]
        ]


viewStep1 : Bool -> Html Msg
viewStep1 done =
    Html.p [ class "mt-8", Attr.classList [ ( "opacity-50", done ) ] ]
        [ Html.span [ class "font-bold" ]
            [ Html.text "Step 1. "
            ]
        , Html.text "Export an "
        , Html.strong [ class "italic font-bold" ] [ Html.text "all-time" ]
        , Html.text " CSV from your Monzo Business account (mobile or web interface)"
        ]


viewStep2 : Bool -> Html Msg
viewStep2 done =
    Html.p [ class "mt-2", Attr.classList [ ( "opacity-50", done ) ] ]
        [ Html.span [ class "font-bold" ]
            [ Html.text "Step 2. "
            ]
        , Html.text "Drop your CSV file onto the box below, or "
        , Html.button [ Events.onClick OpenFilePicker, class "font-bold underline hover hover:text-gray-900" ] [ Html.text "manually select the file" ]
        ]


viewStep3 : Account -> Bool -> Html Msg
viewStep3 account done =
    Html.p
        [ class "mt-2", Attr.classList [ ( "opacity-50", done ) ] ]
        [ Html.span [ class "font-bold" ] [ Html.text "Step 3. " ]
        , Html.text "Is your total bank balance (including pots) exactly "
        , Html.span [ class "font-bold" ] [ Html.text <| viewAmount account.totalAmountInPence ]
        ]


viewDropzone : Bool -> Html Msg
viewDropzone isDraggingOver =
    viewLayout
        [ Html.div
            [ class "container mx-auto px-6" ]
            [ viewPreStep1
            , viewStep1 False
            , viewStep2 False
            , Html.a
                [ Events.preventDefaultOn "dragover" (Decode.succeed ( DragEnter, True ))
                , Events.on "dragleave" (Decode.succeed DragLeave)
                , Events.preventDefaultOn "drop" fileDecoder
                , Events.onClick OpenFilePicker
                , classes
                    [ "bg-gray-800 border-8 border-gray-900"
                    , "hover:bg-gray-700"
                    , "active:bg-gray-800"
                    , "mt-8 cursor-pointer h-64 text-white flex items-center justify-center text-center"
                    ]
                , Attr.classList [ ( "bg-pink-800", isDraggingOver ) ]
                ]
                [ Html.span []
                    [ Html.text "Drop your Monzo business CSV here" ]
                ]
            ]
        ]


viewHeader : Html Msg
viewHeader =
    Html.div [ class "container mx-auto mt-4 mb-6 px-6" ]
        [ Html.a [ Attr.href "/", class "font-display font-bold text-4xl text-white hover:text-gray-900" ]
            [ Html.h1 []
                [ Html.text
                    "monzocruncher"
                ]
            ]
        ]


viewFooter : Html Msg
viewFooter =
    Html.footer
        [ class "container mx-auto px-6 mt-20 mb-12 md:flex" ]
        [ Html.div [ class "md:w-1/2 md:mr-6" ]
            [ Html.h2 [ class "font-bold", Attr.id "notes" ] [ Html.text "Notes" ]
            , Html.p [ class "mt-8" ]
                [ Html.text "This tool provides transaction downloads grouped by month. If you want, for example, to reconcile September’s transactions, it’s best to wait until the beginning of October before importing September’s transactions into Crunch. "
                , Html.text "That way you never run into errors with duplicate transactions in Crunch."
                ]
            , Html.p [ class "mt-8" ]
                [ Html.text "You have to upload an "
                , Html.em [ class "italic" ] [ Html.text "all-time" ]
                , Html.text " CSV export, instead of just a single month, so that the ‘cumulative balance’ column can be calculated."
                ]
            , Html.p [ class "mt-8" ]
                [ Html.text "This tool does not treat Monzo Pots as separate accounts—pot transfer records are not included in the CSV you upload to Crunch."
                ]
            ]
        , Html.div [ class "mt-12 md:mt-0 md:w-1/2 md:ml-6" ]
            [ Html.h2 [ class "font-bold" ] [ Html.text "Important Waffle" ]
            , Html.p [ class "mt-8" ]
                [ Html.text "This is an unofficial hobby project built by "
                , Html.a [ Attr.href "https://mattsenior.com", class "font-bold underline hover:text-gray-900" ] [ Html.text "Matt Senior" ]
                , Html.text " to scratch an itch. Neither Monzo nor Crunch have anything to do with it."
                ]
            , Html.p [ class "mt-8" ]
                [ Html.text "All processing takes place in your browser. None of your data is sent or stored anywhere. All code for the site can be found "
                , Html.a [ Attr.href "https://github.com/mattsenior/monzocruncher", class "font-bold underline hover:text-gray-900" ] [ Html.text "on the GitHub repository" ]
                , Html.text ", which you can clone and run locally if you prefer."
                ]
            , Html.p [ class "mt-8" ]
                [ Html.text "You should have a look through any transactions you download before uploading to Crunch—I’m not responsible for the correctness of the data!"
                ]
            , Html.p [ class "mt-8" ]
                [ Html.text "Monzo changes their CSV format now and again, so this tool will periodically break. I will probably update it within a month because I use it for my own reconciliation, but if you spot any errors, or if you have a file that isn’t processed correctly, please let me know by "
                , Html.a [ Attr.href "https://github.com/mattsenior/monzocruncher/issues", class "font-bold underline hover:text-gray-900" ] [ Html.text "opening a GitHub issue" ]
                , Html.text "."
                ]
            , Html.p [ class "mt-8" ]
                [ Html.text "Credits: Icon from "
                , Html.a [ Attr.href "https://github.com/sschoger/heroicons-ui", class "font-bold underline hover:text-gray-900" ] [ Html.text "Heroicons" ]
                , Html.text ", CSV parser code extended from "
                , Html.a [ Attr.href "https://github.com/periodic/elm-csv", class "font-bold underline hover:text-gray-900" ] [ Html.text "periodic/elm-csv" ]
                , Html.text "."
                ]
            ]
        ]


viewLayout : List (Html Msg) -> Html Msg
viewLayout content =
    Html.main_ [] <| [ viewHeader ] ++ content ++ [ viewFooter ]


viewAccount : Account -> Html Msg
viewAccount account =
    case account.totalConfirmation of
        Pending ->
            viewLayout
                [ Html.div [ class "container mx-auto px-6" ]
                    [ viewPreStep1
                    , viewStep1 True
                    , viewStep2 True
                    , viewStep3 account False
                    , Html.div [ class "mt-8 md:flex md:items-stretch" ]
                        [ Html.button
                            [ Events.onClick ConfirmTotal
                            , classes
                                [ "block w-full md:w-1/2 pt-4 pb-5 md:mr-4 rounded-full text-2xl text-center font-bold"
                                , "bg-white text-gray-900"
                                , "hover:bg-gray-900 hover:text-white"
                                ]
                            ]
                            [ Html.span [ class "md:text-4xl mr-4 md:mr-0" ] [ Html.text "🎉" ]
                            , Html.br [ class "hidden md:inline" ] []
                            , Html.text "Yes it is!"
                            ]
                        , Html.button
                            [ Events.onClick RejectTotal
                            , classes
                                [ "block w-full mt-6 md:mt-0 md:w-1/2 pt-4 pb-5 md:ml-4 rounded-full text-2xl text-center"
                                , "border-2 text-gray-900 border-red-700 hover:bg-red-700 hover:text-white"
                                ]
                            ]
                            [ Html.span [ class "md:text-4xl mr-4 md:mr-0" ] [ Html.text "\u{1F926}\u{200D}♂️" ]
                            , Html.br [ class "hidden md:inline" ] []
                            , Html.text "No, no it’s not"
                            ]
                        ]
                    ]
                ]

        Confirmed ->
            viewAccountPage account

        Rejected ->
            viewLayout
                [ Html.div [ class "container mx-auto px-6" ]
                    [ Html.p []
                        [ Html.span [ class "font-bold" ] [ Html.text "Oh no!" ]
                        , Html.text " Something has clearly gone wrong when importing your transactions."
                        ]
                    , Html.p [ class "mt-8" ]
                        [ Html.text "If you would like to report this as a bug, please "
                        , Html.a [ Attr.href "https://github.com/mattsenior/monzocruncher/issues", class "font-bold underline hover:text-gray-900" ] [ Html.text "open an issue on the GitHub repository" ]
                        , Html.text " and we can chat about it."
                        ]
                    , Html.button [ Events.onClick Reset, class "mt-8 font-bold underline hover:text-gray-900" ] [ Html.text "Start again" ]
                    ]
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
        accountMonth =
            Pivot.getC account.months

        { month, year, records } =
            accountMonth

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
    viewLayout
        [ Html.div [ class "container mx-auto px-6 text-lg" ]
            [ Html.nav [ class "flex justify-between" ] navs
            , Html.h2 [ class "text-center font-semibold text-2xl whitespace-no-wrap" ]
                [ Html.text <| monthToString month ++ " " ++ String.fromInt year
                ]
            , Html.button [ Events.onClick <| DownloadTransactions accountMonth ] [ Html.text "Download transactions" ]
            , Html.table []
                (List.map viewRecord records)
            ]
        ]


viewMonthList : Account -> Html Msg
viewMonthList account =
    let
        viewStatementSummary : AccountMonth -> Html Msg
        viewStatementSummary accountMonth =
            let
                { month, year, records } =
                    accountMonth

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
            Html.li [ class "mt-8 text-xl flex items-center" ]
                [ Html.div [ class "flex-1 md:w-2/3 md:text-right font-bold md:flex" ]
                    [ Html.div [ class "md:w-1/2" ]
                        [ Html.text <|
                            monthToString
                                month
                                ++ " "
                                ++ String.fromInt year
                        ]
                    , Html.div [ class "md:w-1/2 md:text-center font-normal" ]
                        [ Html.text <|
                            String.fromInt
                                numTransactions
                                ++ " "
                                ++ pluralise "transaction"
                                    numTransactions
                        ]
                    ]
                , Html.div [ class "flex-shrink-0 md:w-1/3" ]
                    [ Html.button
                        [ Events.onClick <| DownloadTransactions accountMonth
                        , classes
                            [ "py-3 px-6 rounded-full text-base"
                            , "bg-white text-gray-900 hover:bg-gray-900 hover:text-white"
                            , "flex items-center"
                            ]
                        ]
                        [ Html.span [ class "hidden md:inline font-bold" ]
                            [ Html.text <|
                                "Download "
                            ]
                        , Svg.svg
                            [ SvgAttr.viewBox
                                "0 0 24 24"
                            , SvgAttr.class
                                "md:ml-2 h-5 w-5 fill-current"
                            ]
                            [ Svg.path
                                [ SvgAttr.d "M11 14.59V3a1 1 0 012 0v11.59l3.3-3.3a1 1 0 011.4 1.42l-5 5a1 1 0 01-1.4 0l-5-5a1 1 0 011.4-1.42l3.3 3.3zM3 17a1 1 0 012 0v3h14v-3a1 1 0 012 0v3a2 2 0 01-2 2H5a2 2 0 01-2-2v-3z"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
    in
    viewLayout
        [ Html.div
            [ class "container mx-auto px-6" ]
            [ viewPreStep1
            , viewStep1 True
            , viewStep2 True
            , viewStep3 account True
            , Html.p [ class "mt-2" ]
                [ Html.span [ class "font-bold" ]
                    [ Html.text "Step 4. "
                    ]
                , Html.text "Download the month you need, and import the CSV into Crunch."
                ]
            , Html.ol [ class "mt-12" ]
                (Pivot.toList
                    account.months
                    |> List.map viewStatementSummary
                )
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
