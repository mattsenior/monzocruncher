module Csv exposing
    ( Csv
    , parse
    , parseWith
    , MakeRecordParser
    )

{-| A parser for transforming CSV strings into usable input.

This library does its best to support RFC 4180, however, many CSV inputs do not strictly follow the standard. There are two major deviations assumed in this library.

1.  The `\n` or `\r` character may be used instead of `\r\n` for line separators.
2.  The trailing new-line may be omitted.

RFC 4180 grammar, for reference, with notes.

The trailing newline is required, but we'll make it optional.

    file =
        [ header CRLF ] record * CRLF record [ CRLF ]

    header =
        name * COMMA name

    record =
        field * COMMA field

    name =
        field

    field =
        escaped / non - escaped

There is no room for spaces around the quotes. The specification is that

    escaped =
        DQUOTE * (TEXTDATA / COMMA / CR / LF / 2 DQUOTE) DQUOTE

In this specification, fields that don't have quotes surrounding them cannot have a quote inside them because it is excluded from `TEXTDATA`.

    non-escaped = *TEXTDATA
    COMMA = %x2C
    CR = %x0D ;as per section 6.1 of RFC 2234 [2]
    DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
    LF = %x0A ;as per section 6.1 of RFC 2234 [2]

The spec requires that new lines be `CR + LF` but we'll let them get away with just `LF` if they want..

    CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]

All the printable characters minus the double-quote and comma, this is important above.

    TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E


# Types

@docs Csv


# Functions

@docs parse
@docs parseWith

-}

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompIf
        , chompWhile
        , float
        , getChompedString
        , keyword
        , lazy
        , loop
        , oneOf
        , run
        , succeed
        , symbol
        )
import Result
import String


{-| Represents a CSV document. All CSV documents have a header row, even if that row is empty.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }


{-| Parse a CSV string into its constituent fields, using comma for separator.
-}
parse : MakeRecordParser record -> String -> Result (List Parser.DeadEnd) (List record)
parse makeRecordParser s =
    parseWith makeRecordParser ',' s


{-| Parse a CSV string into its constituent fields, using the passed Char as separator.
-}
parseWith : MakeRecordParser record -> Char -> String -> Result (List Parser.DeadEnd) (List record)
parseWith makeRecordParser c =
    addTrailingLineSep
        >> Parser.run (file makeRecordParser c)


{-| Gets the third element of a tuple.
-}
thrd : ( a, b, c ) -> c
thrd ( _, _, c ) =
    c


{-| carriage return string
-}
crs : String
crs =
    "\u{000D}"


{-| carriage return character
-}
crc : Char
crc =
    '\u{000D}'


{-| Adds a trailing line separator to a string if not present.
-}
addTrailingLineSep : String -> String
addTrailingLineSep str =
    if not (String.endsWith "\n" str || String.endsWith crs str) then
        str ++ crs ++ "\n"

    else
        str


comma : Parser ()
comma =
    symbol ","


doubleQuote : Parser ()
doubleQuote =
    symbol "\""


cr : Parser ()
cr =
    symbol crs


lf : Parser ()
lf =
    symbol "\n"


lineSep : Parser ()
lineSep =
    -- Prefer the multi-character code, but accept others.
    oneOf
        [ backtrackable <| cr |. lf
        , cr
        , lf
        ]


doubleDoubleQuote : Parser ()
doubleDoubleQuote =
    doubleQuote |. doubleQuote


textData : Char -> Parser ()
textData sepChar =
    chompIf <| textChar sepChar


textChar : Char -> Char -> Bool
textChar sepChar c =
    not (List.member c [ '"', sepChar, '\n', crc ])


nonEscaped : Char -> Parser String
nonEscaped sepChar =
    getChompedString (chompWhile (textChar sepChar))


innerChar : Char -> Parser String
innerChar sepChar =
    Parser.map (String.replace "\"\"" "\"") <|
        backtrackable <|
            getChompedString
                (oneOf [ textData sepChar, comma, cr, lf, doubleDoubleQuote ])


innerString : Char -> List String -> Parser (Step (List String) String)
innerString sepChar strs =
    oneOf
        [ succeed (\str -> Loop (str :: strs)) |= innerChar sepChar
        , succeed ()
            |> Parser.map (\_ -> Done (String.concat (List.reverse strs)))
        ]


escaped : Char -> Parser String
escaped sepChar =
    succeed identity
        |. doubleQuote
        |= loop [] (innerString sepChar)
        |. doubleQuote


field : Char -> Parser String
field sepChar =
    oneOf [ escaped sepChar, nonEscaped sepChar ]


name : Char -> Parser String
name sepChar =
    field sepChar


fieldListHelper : Char -> List String -> Parser (Step (List String) (List String))
fieldListHelper sepChar strs =
    oneOf
        [ backtrackable <|
            succeed (\str -> Loop (str :: strs))
                |= field sepChar
                |. symbol (String.fromChar sepChar)
        , succeed (\str -> Done (List.reverse (str :: strs)))
            |= field sepChar
            |. lineSep
        ]


fieldList : Char -> Parser (List String)
fieldList sepChar =
    loop [] (fieldListHelper sepChar)


recordsHelper : Parser record -> List record -> Parser (Step (List record) (List record))
recordsHelper recordParser records =
    oneOf
        [ succeed (\rec -> Loop (rec :: records))
            |= recordParser
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse records))
        ]


type alias MakeRecordParser record =
    { field : Parser String
    , fieldSep : Parser ()
    , lineSep : Parser ()
    }
    -> Parser record


file : MakeRecordParser record -> Char -> Parser (List record)
file makeRecordParser sepChar =
    let
        fieldSep =
            symbol (String.fromChar sepChar)

        recordParser =
            makeRecordParser { field = field sepChar, fieldSep = fieldSep, lineSep = lineSep }
    in
    succeed identity
        |. fieldList sepChar
        |= loop [] (recordsHelper recordParser)
