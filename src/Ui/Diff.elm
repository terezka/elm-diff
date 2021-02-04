module Ui.Diff exposing (Config, viewChunks, viewError)


{-|

This is a library to assist the parsing of `git diff` output and subsequent display.

@docs Config, viewChunks

## Helper
@docs viewError

-}


import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events
import Diff exposing (..)


{-| -}
type alias Config msg =
  { container : List (Attribute msg)
  , line : List (Attribute msg)
  , lineNumber : List (Attribute msg)
  , deleted : List (Attribute msg)
  , added : List (Attribute msg)
  , keyword : List (Attribute msg)
  , constant : List (Attribute msg)
  , customType : List (Attribute msg)
  , definition : List (Attribute msg)
  , buildIn : List (Attribute msg)
  , comment : List (Attribute msg)
  , other : List (Attribute msg)
  }


{-| Display diff chunks. By default it adds two rows of line
numbers on the left, and colors the the line base on whether
the line has been added or removed.

-}
viewChunks : Config msg -> List Chunk -> Html msg
viewChunks config chunks =
  let foldChunk chunk ( ( lnNm1, lnNm2, lns ), lastLnNm ) =
        let defaultAttr =
              [ Attr.style "margin" "0"
              , Attr.style "padding" "0 10px"
              ]

            ( seperator, break ) =
              if lastLnNm + 1 == chunk.lineNum1 || chunk.lineNum1 == 0 then
                ([], [])
              else
                ( [ Html.div (defaultAttr ++ config.lineNumber) [ Html.text "↕" ] ]
                , [ Html.pre (defaultAttr ++ config.line) [ Html.text "\n" ] ]
                )
        in
        ( ( lnNm1 ++ seperator ++ viewLineNums Added .lineNum1 chunk
          , lnNm2 ++ seperator ++ viewLineNums Removed .lineNum2 chunk
          , lns ++ break ++ List.map viewLine chunk.changes
          )
        , chunk.lineNum1 + List.length chunk.changes
        )

      -- LINE #

      viewLineNums skipped toNumber chunk =
        chunk.changes
          |> List.foldl (viewLineNum skipped) ( toNumber chunk, [] )
          |> Tuple.second
          |> List.reverse

      viewLineNum skipped ( action, change ) ( lineNo, elements ) =
        let defaultAttrs =
              [ Attr.style "margin" "0"
              , Attr.style "padding" "0 10px"
              , Attr.style "background-color" (toActionColor action)
              ]

            attrs =
              defaultAttrs ++ config.lineNumber ++ toActionAttrs action

            actionSign =
              case action of
                None -> "·"
                Added -> "+"
                Removed -> "-"

            lineNumText =
              if action == skipped then actionSign
              else String.fromInt lineNo
        in
        ( if action == skipped then lineNo else lineNo + 1
        , Html.div attrs [ Html.text lineNumText ] :: elements
        )

      -- LINE

      viewLine ( action, words ) =
        let defaultAttrs =
              [ Attr.style "margin" "0"
              , Attr.style "padding" "0 5px"
              , Attr.style "width" "100%"
              , Attr.style "background-color" (toActionColor action)
              ]

            attrs =
              defaultAttrs ++ config.line ++ toActionAttrs action
        in
        Html.pre attrs <|
          case words of
            [] -> [ Html.text "\n" ]
            _ -> List.map viewWord words

      viewWord word =
        let element color toAttrs str =
              Html.span (Attr.style "color" color :: toAttrs config) [ Html.text str ]
        in
        case word of
          Keyword str -> element "#058eda" .keyword str
          Constant str -> element "#e43882" .constant str
          CustomType str -> element "#d7670a" .customType  str
          Definiton str -> element "#045ca4" .definition str
          BuildIn str -> element "#045ca4" .buildIn str
          Comment str -> element "#666" .comment str
          Other str -> element "#666" .other str

      ( ( lineNumsLeft, lineNumsRight, lines ), _ ) =
        List.foldl foldChunk ( ( [], [], [] ), 0 ) chunks

      -- HELP

      toActionAttrs action =
        case action of
          None -> []
          Added -> config.added
          Removed -> config.deleted

      toActionColor action =
        case action of
          None -> "transparent"
          Removed -> "#ffe7e7"
          Added -> "#e9feed"
  in
  Html.div
    ([ Attr.style "font-size" "14px"
    , Attr.style "line-height" "1.25"
    , Attr.style "width" "100%"
    , Attr.style "display" "flex"
    ] ++ config.container)
    [ Html.pre
        [ Attr.style "display" "inline-block"
        , Attr.style "margin" "0"
        , Attr.style "text-align" "right"
        , Attr.style "color" "#aaa"
        , Attr.style "vertical-align" "top"
        ]
        lineNumsLeft
    , Html.pre
        [ Attr.style "display" "inline-block"
        , Attr.style "margin" "0"
        , Attr.style "text-align" "right"
        , Attr.style "color" "#aaa"
        , Attr.style "vertical-align" "top"
        ]
        lineNumsRight
    , Html.pre
        [ Attr.style "display" "inline-block"
        , Attr.style "width" "100%"
        , Attr.style "margin" "0"
        , Attr.style "vertical-align" "top"
        ]
        lines
    ]


{-| Helper to view parsing errors.

-}
viewError : Error -> Html msg
viewError error =
  let viewLine index line =
        Html.pre [ Attr.style "margin" "0" ]
          [ Html.span
              [ Attr.style "margin" "0"
              , Attr.style "width" "30px"
              , Attr.style "padding" "10px"
              ]
              [ Html.text (String.fromInt <| index + 1) ]
          , Html.text line
          ]
  in
  Html.div
    [ Attr.style "margin" "0"
    , Attr.style "width" "100%"
    , Attr.style "font-size" "14px"
    , Attr.style "line-height" "1.25"
    ]
    [ Html.p [] [ Html.text error.message ]
    , Html.pre
      [ Attr.style "margin" "0"
      , Attr.style "width" "100%"
      ]
      (List.indexedMap viewLine (String.split "\n" error.subject))
    ]


