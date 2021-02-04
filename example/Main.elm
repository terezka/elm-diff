module Main exposing (..)


import Html
import Html.Attributes as Attr
import Diff
import Ui.Diff


main =
  Html.div
    [ Attr.style "width" "100%"
    , Attr.style "max-width" "800px"
    , Attr.style "margin" "50px auto"
    ]
    [ Html.h1 [] [ Html.text "elm diff" ]
    , Html.p []
        [ Html.text "Elm helpers for processing and displaying "
        , Html.code [] [ Html.text "`git diff`" ]
        , Html.text " output. Read the full documentation "
        , Html.a
            [ Attr.href "https://package.elm-lang.org/packages/terezka/elm-diff/latest"
            , Attr.style "color" "rgb(5, 142, 218)"
            , Attr.style "text-decoration" "none"
            ]
            [ Html.text "here" ]
        , Html.text "."
        ]
    , Html.p
        [ Attr.style "text-align" "center"
        , Attr.style "margin" "30px 0"
        ]
        [ Html.code
            [ Attr.style "padding" "5px 10px"
            , Attr.style "background" "whitesmoke"
            , Attr.style "border-radius" "5px"
            ]
            [ Html.text "$ elm install terezka/elm-diff" ]
        ]
    , case Diff.fromString exampleDiff of
        Ok diffs ->
          Html.div
            [ Attr.style "width" "100%"
            , Attr.style "max-width" "800px"
            , Attr.style "margin" "0 auto"
            ]
            (List.map (Diff.toChunks >> Ui.Diff.viewChunks config) diffs)

        Err err ->
          Ui.Diff.viewError err
    , Html.p [] [ Html.text "Written by Tereza Sokol." ]
    ]


config =
  { container = []
  , line = []
  , lineNumber = []
  , deleted = []
  , added = []
  , keyword = []
  , constant = []
  , customType = []
  , definition = []
  , buildIn = []
  , comment = []
  , other = []
  }


exampleDiff : String
exampleDiff =
  "\ndiff --git a/src/Example/Clock.elm b/src/Example/Clock.elm\nindex a740f0f..136b71b 100644\n--- a/src/Example/Clock.elm\n+++ b/src/Example/Clock.elm\n@@ -33,7 +33,7 @@ type alias Model =\n \n init : () -> (Model, Cmd Msg)\n init _ =\n-  ( Model Time.utc (Time.millisToPosix 0)\n+  ( Model Time.utc (Time.millisToPosix 1000)\n   , Cmd.batch\n       [ Task.perform AdjustTimeZone Time.here\n       , Task.perform Tick Time.now\n@@ -55,14 +55,10 @@ update : Msg -> Model -> (Model, Cmd Msg)\n update msg model =\n   case msg of\n     Tick newTime ->\n-      ( { model | time = newTime }\n-      , Cmd.none\n-      )\n+      ( { model | time = newTime }, Cmd.none )\n \n     AdjustTimeZone newZone ->\n-      ( { model | zone = newZone }\n-      , Cmd.none\n-      )\n+      ( { model | zone = newZone }, Cmd.none )\n \n \n \n@@ -80,10 +76,9 @@ subscriptions model =\n \n view : Model -> Html Msg\n view model =\n-  let\n-    hour   = toFloat (Time.toHour   model.zone model.time)\n-    minute = toFloat (Time.toMinute model.zone model.time)\n-    second = toFloat (Time.toSecond model.zone model.time)\n+  let hour   = toFloat (Time.toHour   model.zone model.time)\n+      minute = toFloat (Time.toMinute model.zone model.time)\n+      second = toFloat (Time.toSecond model.zone model.time)\n   in\n   svg\n     [ viewBox \"0 0 400 400\"\n@@ -91,14 +86,14 @@ view model =\n     , height \"400\"\n     ]\n     [ circle [ cx \"200\", cy \"200\", r \"120\", fill \"#1293D8\" ] []\n-    , viewHand 6 60 (hour/12)\n-    , viewHand 6 90 (minute/60)\n-    , viewHand 3 90 (second/60)\n+    , viewTicker 6 60 (hour / 12)\n+    , viewTicker 6 90 (minute / 60)\n+    , viewTicker 3 90 (second / 60)\n     ]\n \n \n-viewHand : Int -> Float -> Float -> Svg msg\n-viewHand width length turns =\n+viewTicker : Int -> Float -> Float -> Svg msg\n+viewTicker width length turns =\n   let\n     t = 2 * pi * (turns - 0.25)\n     x = 200 + length * cos t\n@@ -109,7 +104,7 @@ viewHand width length turns =\n     , y1 \"200\"\n     , x2 (String.fromFloat x)\n     , y2 (String.fromFloat y)\n-    , stroke \"white\"\n+    , stroke \"pink\"\n     , strokeWidth (String.fromInt width)\n     , strokeLinecap \"round\"\n     ]\n"
