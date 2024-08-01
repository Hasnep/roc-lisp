module [trimStart, expectOk, expectErr]

# trimStart : List U8 -> List U8
# trimStart = \list ->
#     when list is
#         [' ', .. as rest] -> trimStart rest
#         ['\n', .. as rest] -> trimStart rest
#         _ -> list

trimStart : Str -> Str
trimStart = Str.trimStart

expectOk : Result _ b, _ -> Bool
expectOk = \result, expected ->
    when result is
        Ok value -> value == expected
        Err _ -> Bool.false

expectErr : Result a _, _ -> Bool
expectErr = \result, expected ->
    when result is
        Ok _ -> Bool.false
        Err value -> value == expected
