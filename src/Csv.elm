module Csv exposing
    ( Csv
    , MakeRecordParser
    , parse
    , parseWith
    )

-- This CSV parsing code is an extension of https://github.com/periodic/elm-csv

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
