# elm-diff

A parser for `git diff` in Elm. Also allows for highlighting of Elm / JSON files.

## Usage

Use `Diff.fromString` to parse `git diff` output.

```
import Diff exposing (Diff)

parsed : Result Diff.Error Diff
parsed =
  Diff.fromString original

original : String
original =
  """\ndiff --git a/src/Hello.elm b/src/Hello.elm\nnew file mode 100644\nindex 0000000..5482d42\n--- /dev/null\n+++ b/src/Hello.elm\n@@ -0,0 +1,4 @@\n+module Hello exposing (..)\n+\n+\n+hello = 1\n\ No newline at end of file"""
```

## Install

```
$ elm install terezka/elm-diff
```