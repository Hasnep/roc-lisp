app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/cf_TpThUd4e69C7WzHxCbgsagnDmk3xlb_HmEKXTICw.tar.br",
    lisp: "../lib/main.roc",
}

import cli.Arg
import cli.Stdout
import cli.Task
import lisp.Lisp

run : Task.Task {} _
run =
    input = Arg.list!
    result = input |> Str.joinWith " " |> Str.toUtf8 |> Lisp.parse
    when result is
        Ok values -> values |> List.map Lisp.display |> Str.joinWith " " |> Stdout.line
        Err err -> Task.err err

handleErr : _ -> [Exit I32 Str]
handleErr = \err ->
    when err is
        Exit i s -> Exit i s
        EmptyList -> Exit 2 "Empty list."
        IncorrectArity { expected, got } -> Exit 3 "Incorrect arity, expected $(Num.toStr (Num.toU64 expected)), got $(Num.toStr (Num.toU64 got))."
        MissingProcedure -> Exit 4 "Missing procedure."
        TypeError { expected, got, value } -> Exit 5 "Type error, expected $(expected), got $(got), value $(value)."
        UnexpectedCloseParen -> Exit 6 "Unexpected close paren."
        UnexpectedEndOfFile -> Exit 7 "Unexpected end of file."
        UnknownValue value -> Exit 8 "Unknown value $(value)."
        StdoutErr _ -> Exit 1 "Failed to write to stdout."

main : Task.Task {} [Exit I32 Str]
main = Task.mapErr run handleErr
