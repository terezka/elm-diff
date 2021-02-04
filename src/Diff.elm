module Diff exposing
  ( Diff(..), NewFile, DeletedFile, EditedFile, RenamedFile
  , Chunk, Word(..), Action(..), Hash, Path
  , fromString, Error, toPath, toChunks
  )


{-|

This is a library to assist the parsing of `git diff` output and subsequent display.

## Parser
@docs fromString

## Data model
@docs Diff, NewFile, DeletedFile, EditedFile, RenamedFile, Chunk, Word, Action, Hash, Path, Error

## Helpers
@docs toPath, toChunks

-}

import Parser exposing (..)


{-| A diff for a single file. There may be several
of these in a single `git diff` output.

_Note_: This does not include all possible modes,
like changing permissions of file, symlinks etc. If you
need that though, you're welcome to add it.

-}
type Diff
  = New NewFile
  | Deleted DeletedFile
  | Edited EditedFile
  | Renamed RenamedFile


{-| -}
type alias NewFile =
  { path : Path
  , commit : Hash
  , chunks : List Chunk
  }


{-| -}
type alias DeletedFile =
  { path : Path
  , commit : Hash
  , chunks : List Chunk
  }


{-| -}
type alias EditedFile =
  { path : Path
  , commit1 : Hash
  , commit2 : Hash
  , chunks : List Chunk
  }


{-| -}
type alias RenamedFile =
  { from : Path
  , to : Path
  , similarity : Int
  , commit1 : Hash
  , commit2 : Hash
  , chunks : List Chunk
  }


{-| -}
type alias Chunk =
  { lineNum1 : Int
  , lineCount1 : Int
  , lineNum2 : Int
  , lineCount2 : Int
  , changes : List ( Action, List Word )
  }


{-| -}
type alias Hash =
  String


{-| -}
type alias Path =
  String


{-| -}
type Action
  = None
  | Added
  | Removed


{-| The diff is parsed as Elm code. This incidentally
also works fairly well for JSON. For any other file extensions
other than .elm and .json, the line of code is just left in its
entirety in the `Other` type.

Since we're working with diffs, this is meant to parse
incomplete code. There are thus certain limits for highlighting
as we don't have access to the full code. E.g. it is difficult
(sometimes impossible) to highlight multiline comments / quotes
correctly. This has thus been omitted.

-}
type Word
  = Keyword String
  | Constant String
  | CustomType String
  | Definiton String
  | BuildIn String
  | Comment String
  | Other String



-- API


{-| Helper to get the path for the diff. In the case
of renames, this is the latest path.

-}
toPath : Diff -> Path
toPath diff =
  case diff of
    New { path } -> path
    Deleted { path } -> path
    Edited { path } -> path
    Renamed { to } -> to


{-| Helper to get the diff chunks.

-}
toChunks : Diff -> List Chunk
toChunks diff =
  case diff of
    New { chunks } -> chunks
    Deleted { chunks } -> chunks
    Edited { chunks } -> chunks
    Renamed { chunks } -> chunks



-- PARSER


{-| Parse the full output of `git diff`.

-}
fromString : String -> Result Error (List Diff)
fromString str =
  run parser str
    |> Result.mapError (Error str << String.join "\n" << List.map deadEndToString)


type alias Error =
  { subject : String
  , message : String
  }



-- INTERNAL


parser : Parser (List Diff)
parser =
  succeed identity
    |. spaces
    |= parseDiffs
    |. spaces
    |. end


parseDiffs : Parser (List Diff)
parseDiffs =
  loop [] <| \acc ->
    oneOf
      [ succeed (\one -> Loop (one :: acc))
          |= parseDiff
      , succeed ()
          |> map (\_ -> Done (List.reverse acc))
      ]


parseDiff : Parser Diff
parseDiff =
  succeed (\a b mode -> ( mode a b, b))
    |. symbol "diff --git a/"
    |= getChompedString (chompWhile (\c -> c /= ' '))
    |. symbol " b/"
    |= getChompedString (chompWhile (\c -> c /= '\n'))
    |. symbol "\n"
    |= parseMode
    |> andThen (\( f, path ) ->
        let isElmOrJson = String.endsWith ".elm" path || String.endsWith ".json" path in
        succeed f |= parseChunks isElmOrJson
      )


parseMode : Parser (Path -> Path -> List Chunk -> Diff)
parseMode =
  oneOf
    [ succeed (\hash path _ chunks -> New (NewFile path hash chunks))
        |. symbol "new file mode"
        |. untilNewLine
        |. symbol "index "
        |. chompWhile Char.isAlphaNum
        |. symbol ".."
        |= parseHash
        |. untilNewLine
        |. oneOf
            [ symbol "--- /dev/null"
                |. untilNewLine
                |. symbol "+++ b/"
                |. untilNewLine
            , succeed ()
            ]
    , succeed (\hash path _ chunks -> Deleted (DeletedFile path hash chunks))
        |. symbol "deleted file mode"
        |. untilNewLine
        |. symbol "index "
        |= parseHash
        |. untilNewLine
        |. oneOf
            [ symbol "--- a/"
                |. untilNewLine
                |. symbol "+++ /dev/null"
                |. untilNewLine
            , succeed ()
            ]
    , succeed (\sim hash1 hash2 path1 path2 chunks -> Renamed (RenamedFile path1 path2 sim hash1 hash2 chunks))
        |. symbol "similarity index "
        |= int
        |. untilNewLine
        |. symbol "rename from "
        |. untilNewLine
        |. symbol "rename to "
        |. untilNewLine
        |. symbol "index "
        |= parseHash
        |. symbol ".."
        |= parseHash
        |. untilNewLine
        |. symbol "--- a/"
        |. untilNewLine
        |. symbol "+++ b/"
        |. untilNewLine
    , succeed (\hash1 hash2 path _ chunks -> Edited (EditedFile path hash1 hash2 chunks))
        |. symbol "index "
        |= parseHash
        |. symbol ".."
        |= parseHash
        |. untilNewLine
        |. symbol "--- a/"
        |. untilNewLine
        |. symbol "+++ b/"
        |. untilNewLine
    ]


parseHash : Parser Hash
parseHash =
  getChompedString (chompWhile Char.isAlphaNum)


parseChunks : Bool -> Parser (List Chunk)
parseChunks isElmOrJson =
  loop [] <| \acc ->
    oneOf
      [ succeed Chunk
          |. symbol "@@ -"
          |= int
          |= oneOf [ succeed identity |. symbol "," |= int, succeed 1 ]
          |. symbol " +"
          |= int
          |= oneOf [ succeed identity |. symbol "," |= int, succeed 1 ]
          |. symbol " @@"
          |. chompUntilEndOr "\n"
          |. oneOf [ symbol "\n", succeed () ]
          |= parseChanges isElmOrJson
          |> map (\one -> Loop (one :: acc))
      , succeed ()
          |> map (\_ -> Done (List.reverse acc))
      ]


parseChanges : Bool -> Parser (List ( Action, List Word ))
parseChanges isElmOrJson =
  loop [] <| \acc ->
    oneOf
      [ succeed ()
          |. symbol "\\ No newline at end of file"
          |. spaces
          |> map (\_ -> Loop acc)
      , succeed Tuple.pair
          |= oneOf
              [ succeed None
                  |. symbol " "
              , succeed Added
                  |. symbol "+"
              , succeed Removed
                  |. symbol "-"
              ]
          |= parseLine isElmOrJson
          |> map (\one -> Loop (one :: acc))
      , succeed ()
          |> map (\_ -> Done (List.reverse acc))
      ]


parseLine : Bool -> Parser (List Word)
parseLine isElmOrJson =
  if isElmOrJson then parseElmLine
  else map (Other >> List.singleton) untilNewLine


untilNewLine : Parser String
untilNewLine =
  getChompedString (chompUntilEndOr "\n")
    |. oneOf [ symbol "\n", succeed () ]


parseElmLine : Parser (List Word)
parseElmLine =
  let keywords =
        [ "if", "then", "else", "case", "of"
        , "let", "in", "type", "alias", "module", "where"
        , "import", "exposing", "as", "port"
        ]

      buildins =
        [ "==", "/=", "::", "->", ",", ".", "(", ")"
        , "{", "}", "=", "|", ":", "[", "]" ]

      continue acc toType =
        getChompedString >> map (\a -> Loop (toType a :: acc))

      parseWord acc =
        oneOf <|
          List.map (keyword >> continue acc Keyword) keywords ++
          List.map (symbol >> continue acc BuildIn) buildins ++
          [ -- NUMBERS
            succeed identity
              |. chompIf Char.isDigit
              |. chompWhile Char.isDigit
              |. oneOf
                  [ succeed ()
                      |. chompIf (\c -> c == '.')
                      |. chompWhile Char.isDigit
                  , succeed ()
                  ]
              |> continue acc Constant

            -- COMMENT
          , lineComment "--"
              |> continue acc Comment

            -- CUSTOM TYPE / MODULE QUALIFIER
          , succeed identity
              |. chompIf Char.isUpper
              |. chompWhile (\c -> Char.isAlphaNum c || c == '_')
              |> getChompedString
              |> continue acc CustomType

            -- MULTILINE TEXT
          , symbol "\"\"\""
              |. chompWhile (\c -> c /= '"' && c /= '\n')
              |. oneOf
                  [ symbol "\"\"\""
                  , succeed ()
                  ]
              |> continue acc Other

            -- SINGLE LINE TEXT
          , succeed identity
              |. chompIf (\c -> c == '"')
              |. chompWhile (\c -> c /= '"' && c /= '\n')
              |. oneOf
                  [ symbol "\""
                  , succeed ()
                  ]
              |> continue acc Constant

            -- SPACES
          , succeed identity
              |. chompIf (\c -> c == ' ')
              |. chompWhile (\c -> c == ' ')
              |> continue acc Other

            -- OTHER
          , succeed identity
              |. chompIf (\c -> c /= '\n' && c /= ' ')
              |. chompWhile (\c -> c /= '\n' && c /= ' ')
              |> continue acc Other

            -- END
          , symbol "\n"
              |> map (\_ -> Done <| List.reverse acc)
          ]
  in
  succeed (\a b -> a ++ b)
    |= oneOf
        [ -- DEFINITIONS
          succeed identity
            |. chompIf Char.isLower
            |. chompWhile Char.isAlphaNum
            |> getChompedString
            |> map (\a -> if List.member a keywords then Keyword a else Definiton a)
            |> map List.singleton

        , succeed []
        ]
    |= loop [] parseWord



-- HELPERS


deadEndToString : DeadEnd -> String
deadEndToString deadEnd =
  "Problem at row " ++ String.fromInt deadEnd.row ++
  " column " ++ String.fromInt deadEnd.col ++ ": " ++
  problemToString deadEnd.problem


problemToString : Problem -> String
problemToString problem=
  case problem of
    Expecting str -> "Expecting \"" ++ str ++ "\"."
    ExpectingInt -> "Expecting an integer."
    ExpectingHex -> "Expecting a hex number."
    ExpectingOctal -> "Expecting a octal number."
    ExpectingBinary -> "Expecting a binary number."
    ExpectingFloat -> "Expecting a float."
    ExpectingNumber -> "Expecting a number."
    ExpectingVariable -> "Expecting a variable."
    ExpectingSymbol str -> "Expecting the symbol \"" ++ str ++ "\"."
    ExpectingKeyword str -> "Expecting the keyword \"" ++ str ++ "\"."
    ExpectingEnd -> "Expecting the end."
    UnexpectedChar -> "Did not expect character."
    Problem str -> str
    BadRepeat -> "Bad repeat."