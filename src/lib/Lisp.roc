module [eval, parse, display]

import Utils exposing [trimStart, expectOk, expectErr]

import Unicode.Grapheme

Expr : [
    ListExpr (List Expr),
    BoolExpr Bool,
    IntExpr I64,
    AtomExpr Str,
    ProcedureExpr Procedure,
    ClosureExpr { arguments : List Str, body : List Expr, scope : Scope },
]

## Error represents a failure in the language.
## Different types of errors are represented as variants.
Error : [
    UnknownValue Str,
    MissingProcedure,
    IncorrectArity { expected : U64, got : U64 },
    TypeError { expected : Str, got : Str, value : Str },
    EmptyList,
    UnexpectedEndOfFile,
    UnexpectedCloseParen,
]

## Scope represents a mapping of names to values.
Scope : Dict Str Expr

## State represents the state of the interpreter.
## It contains the global scope and the local scope.
## The local scope is used for local variables and the global scope is used for global variables.
State : { globalScope : Scope, localScope : Scope }

## A type alias for the result of evaluating an expression.
Evaluated : Result (Expr, State) Error

## A type alias for the result of parsing an expression.
Parsed : Result (Expr, Str) Error

## A type alias for a procedure.
Procedure : List Expr, State -> Evaluated

## Eval function takes a string as an input and returns a result after evaluating the input.
eval : Str -> Result Str Error
eval = \source ->
    source
    |> parse []
    |> Result.try (\expressions -> evaluate expressions (ListExpr []) (newState {}))
    |> Result.map (\(expr, _state) -> expr)
    |> Result.map display

empty : [ListExpr (List Expr)]
empty = ListExpr []

# -- Parsing --

## Parse function takes a string as an input and a list of expressions and returns a result after parsing the input.
parse : Str, List Expr -> Result (List Expr) Error
parse = \source, expressions ->
    source
    |> parseExpression
    |> Result.try
        (
            \(expression, rest) ->
                expressions2 = List.concat [expression] expressions
                when trimStart rest is
                    "" -> Ok (List.reverse expressions2)
                    _ -> parse rest expressions2
        )

## Takes a string as a source and returns a result after parsing the expressions included in the source.
## Also checks for unexpected end of file and unexpected close parenthesis.
parseExpression : Str -> Parsed
parseExpression = \source ->
    source2 = trimStart source
    when Unicode.Grapheme.split source2 is
        Ok [] -> Err UnexpectedEndOfFile
        Ok [")", ..] -> Err UnexpectedCloseParen
        Ok ["(", .. as rest] -> parseList (Str.joinWith rest "")
        Ok rest -> parseAtom (Str.joinWith rest "")
        Err _ -> crash "Oh no!"

## Take a string as a source and return a result.
parseList : Str -> Parsed
parseList = \source -> tailRecursiveParseList source []

tailRecursiveParseList : Str, List Expr -> Parsed
tailRecursiveParseList = \source, elements ->
    source2 = trimStart source
    when Unicode.Grapheme.split source2 is
        Ok [] -> Err UnexpectedEndOfFile
        Ok [")", .. as rest] -> Ok (ListExpr (List.reverse elements), Str.joinWith rest "")
        Ok _ ->
            parseExpression source
            |> Result.try
                (
                    \(expression, rest) ->
                        tailRecursiveParseList rest (List.concat [expression] elements)
                )

        Err _ -> crash "Oh no!"

## Parse_atom function takes a string as a source and returns a result Atoms can be integers, booleans or symbols. This function also checks for unexpected end of file and unexpected close parenthesis.
parseAtom : Str -> Parsed
parseAtom = \source ->
    (content, rest) = parseAtomContent source ""
    when (content, Unicode.Grapheme.split rest) is
        ("", Ok []) -> Err UnexpectedEndOfFile
        ("", Ok [")", ..]) -> Err UnexpectedCloseParen
        ("true", Ok _) -> Ok (BoolExpr Bool.true, rest)
        ("false", Ok _) -> Ok (BoolExpr Bool.false, rest)
        (_, Ok _) ->
            atom =
                when Str.toI64 content is
                    Ok atom2 -> IntExpr atom2
                    Err _ -> AtomExpr content
            Ok (atom, rest)

        (_, Err _) -> crash "Oh no!"

## Parse_atom_content function parses the content of an atom.
## It takes a string as source and an atom and returns a tuple.
parseAtomContent : Str, Str -> (Str, Str)
parseAtomContent = \source, atom ->
    (char, rest) =
        when Unicode.Grapheme.split source is
            Ok [char2, .. as rest2] -> (char2, Str.joinWith rest2 "")
            _ -> ("", "")
    when char is
        "" -> (atom, source)
        ")" -> (atom, source)
        " " -> (atom, rest)
        _ -> parseAtomContent rest (Str.concat atom char)

# -- State --

## New_state function creates a new state for the interpreter.
## It initializes the global scope with the built-in procedures.
## It also initializes the local scope with an empty dictionary.
newState : {} -> State
newState = \{} ->
    globalScope : Scope
    globalScope =
        Dict.fromList [
            ("+", makeIntOperator (\a, b -> a + b) 0),
            ("-", makeIntOperator (\a, b -> a - b) 0),
            ("*", makeIntOperator (\a, b -> a * b) 1),
            ("/", makeIntOperator (\a, b -> a // b) 1),
            # ("empty", empty),
            ("cons", ProcedureExpr consBuiltin),
            ("car", ProcedureExpr carBuiltin),
            ("cdr", ProcedureExpr cdrBuiltin),
            ("let", ProcedureExpr letBuiltin),
            ("=", ProcedureExpr eqBuiltin),
            ("not", ProcedureExpr notBuiltin),
            ("and", ProcedureExpr andBuiltin),
            ("or", ProcedureExpr orBuiltin),
            ("if", ProcedureExpr ifBuiltin),
            ("define", ProcedureExpr defineBuiltin),
            ("lambda", ProcedureExpr lambdaBuiltin),
        ]
    localScope = Dict.empty {}
    { globalScope, localScope }

# -- Evaluation --

## Evaluate function takes a list of expressions, an accumulator and a state and returns a result after evaluating the expressions.
evaluate : List Expr, Expr, State -> Evaluated
evaluate = \expressions, accumulator, state ->
    when expressions is
        [] -> Ok (accumulator, state)
        [expression, .. as rest] ->
            evaluateExpression expression state
            |> Result.try (\(evaluated, state2) -> evaluate expressions evaluated state2)

## Evaluate_expression function takes an expression and a state and returns a result after evaluating the expression.
evaluateExpression : Expr, State -> Evaluated
evaluateExpression = \expression, state ->
    when expression is
        BoolExpr _ | IntExpr _ | ProcedureExpr _ | ClosureExpr _ -> Ok (expression, state)
        ListExpr expressions -> evaluateList expressions state
        AtomExpr atom -> evaluateAtom atom state |> Result.try \value -> Ok (value, state)

## Evaluate_expressions function takes a list of expressions, an evaluated list
## and a state and returns a result after evaluating the expressions.
evaluateExpressions : List Expr, List Expr, State -> Result (List _, _) _
evaluateExpressions = \expressions, evaluated, state ->
    when expressions is
        [] -> Ok (List.reverse evaluated, state)
        [expression, .. as rest] ->
            evaluateExpression expression state
            |> Result.try
                (
                    \(expression2, state2) ->
                        evaluateExpressions rest (List.concat [expression2] evaluated) state2
                )

## Evaluate_list function takes a list of expressions and a state and returns a result after evaluating the list.
## Also checks for missing procedure.
evaluateList : List Expr, State -> Evaluated
evaluateList = \list, state ->
    when list is
        [] -> Err MissingProcedure
        [procedure, .. as arguments] ->
            evaluateExpression procedure state
            |> Result.try (\(procedure2, state2) -> call procedure2 arguments state2)

## Call function takes a callable, a list of arguments and a state and returns a result after calling the callable with the arguments.
## Also checks for type error.
call : Expr, List Expr, State -> Evaluated
call = \callable, arguments, state ->
    when callable is
        ProcedureExpr procedure -> procedure arguments state
        ClosureExpr { arguments: arguments2, body, scope } -> callClosure arguments2 body scope arguments state
        _ -> Err (typeError "Procedure" callable)

## Call_closure function takes a list of parameters, a list of expressions, a dictionary of environment, a list of arguments and a state and returns a
callClosure : List Str, List Expr, Scope, List Expr, State -> Evaluated
callClosure = \parameters, body, environment, arguments, state ->
    originalLocals = state.localScope
    state2 = setLocals state environment
    evaluateLambdaArguments parameters arguments state2 0
    |> Result.try
        (
            \state3 ->
                evaluate body (ListExpr []) state3
                |> Result.try \(result, state4) -> Ok (result, setLocals state4 originalLocals)
        )

## Evaluate_lambda_arguments function takes a list of parameters, a list of arguments, a state and a count and returns a result after evaluating the lambda arguments. Also checks for incorrect arity.
evaluateLambdaArguments : List Str, List Expr, State, U64 -> Result State Error
evaluateLambdaArguments = \parameters, arguments, state, count ->
    when (parameters, arguments) is
        ([], []) -> Ok state
        ([], rest) -> Err (IncorrectArity { expected: count, got: count + List.len rest })
        (rest, []) -> Err (IncorrectArity { expected: count + List.len rest, got: count })
        ([parameter, .. as parameters2], [argument, .. as arguments2]) ->
            evaluateExpression argument state
            |> Result.try
                (
                    \(argument2, state2) ->
                        evaluateLambdaArguments parameters2 arguments2 (insertLocal state2 parameter argument2) (count + 1)
                )

## Set_locals function takes a state and a scope and returns a new state with the local scope set to the given scope.
setLocals : State, Scope -> State
setLocals = \state, locals -> { state & localScope: locals }

## Insert_local function takes a state, a name and a value and returns a new state with the local scope updated with the given name and value.
insertLocal : State, Str, Expr -> State
insertLocal = \state, name, value -> { state & localScope: state.localScope |> Dict.insert name value }

## Insert_global function takes a state, a name and a value and returns a new state with the global scope updated with the given name and value.
insertGlobal : State, Str, Expr -> State
insertGlobal = \state, name, value -> { state & globalScope: state.globalScope |> Dict.insert name value }

# -- Built-in procedures --

## Define_builtin function takes a list of expressions and a state and returns a result after defining a global variable. Also checks for incorrect arity.
defineBuiltin : List Expr, State -> Evaluated
defineBuiltin = \arguments, state ->
    when arguments is
        [name, value] ->
            expectAtom name
            |> Result.try
                (\name2 -> evaluateExpression value state
                    |> Result.try (\(value2, state2) -> Ok (ListExpr [], insertGlobal state2 name2 value2)))

        _ -> Err (arityError 2 arguments)

## Lambda_builtin function takes a list of expressions and a state and returns a result after defining a procedure. Also checks for incorrect arity.
lambdaBuiltin : List Expr, State -> Evaluated
lambdaBuiltin = \arguments, state ->
    when arguments is
        [parameters, .. as body] ->
            expectList parameters
            |> Result.try
                (
                    \parameters2 ->
                        List.mapTry parameters2 expectAtom
                        |> Result.try
                            (
                                \parameters3 -> Ok (ClosureExpr { arguments: parameters3, body, scope: state.localScope }, state)
                            )
                )

        _ -> Err (arityError 2 arguments)

## Evaluate_atom function takes an atom and a state and returns a result after evaluating the atom. Also checks for unknown value.
evaluateAtom : Str, State -> Result Expr Error
evaluateAtom = \atom, state ->
    state.localScope
    |> Dict.get atom
    |> Result.mapErr (\_ -> Dict.get state.globalScope atom)
    |> Result.mapErr (\_ -> UnknownValue atom)

## Make_int_operator function is responsible for creating a procedure that takes a list of integers and returns a result after reducing the list. Used for the built-in procedures +, -, * and /
makeIntOperator : (I64, I64 -> I64), I64 -> [ProcedureExpr (List Expr, State -> Result (Expr, State) Error)]
makeIntOperator = \reducer, initial ->
    procedure =
        \values, state ->
            evaluateExpressions values [] state
            |> Result.try
                (
                    \(values2, state2) ->
                        List.mapTry values2 expectInt
                        |> Result.try
                            (
                                \ints ->
                                    Ok (
                                        IntExpr (List.walk ints initial reducer),
                                        state2,
                                    )
                            )
                )
    ProcedureExpr procedure

## Cons_builtin function takes a list of expressions and a state and returns a result after consing the expressions. Also checks for incorrect arity.
consBuiltin : List Expr, State -> Evaluated
consBuiltin = \values, state ->
    evaluateExpressions values [] state
    |> Result.try \(values2, state2) ->
        when values is
            [head, tail] ->
                expectList tail
                |> Result.try
                    (
                        \tail2 -> Ok (ListExpr (List.concat [head] tail2), state2)
                    )

            _ -> Err (arityError 2 values2)

## Car_builtin function takes a list of expressions and a state and returns a result after getting the first element of the list. Returns an error if the list is empty.
carBuiltin : List Expr, State -> Evaluated
carBuiltin = \expressions, state ->
    expect1 expressions
    |> Result.try
        (\expression ->
            evaluateExpression expression state
            |> Result.try
                (\(value, state2) ->
                    expectList value
                    |> Result.try
                        (\list ->
                            when list is
                                [] -> Err EmptyList
                                [head, ..] -> Ok (head, state2)
                        )
                )
        )

## Cdr_builtin function takes a list of expressions and a state and returns a result after getting the rest of the list. Returns an error if the list is empty.
cdrBuiltin : List Expr, State -> Evaluated
cdrBuiltin = \expressions, state ->
    expect1 expressions
    |> Result.try
        (\expression ->
            evaluateExpression expression state
            |> Result.try
                (\(value2, state2) ->
                    expectList value2
                    |> Result.try
                        (\list ->
                            when list is
                                [] -> Err EmptyList
                                [_, .. as tail] -> Ok (ListExpr tail, state2)
                        )
                )
        )

## Let_builtin function takes a list of expressions and a state and returns a result after defining a local variable.
letBuiltin : List Expr, State -> Evaluated
letBuiltin = \expressions, state ->
    originalLocals = state.localScope
    expect2 expressions
    |> Result.try
        (\(bindings, value) -> expectList bindings
            |> Result.try
                (\bindings2 -> List.walkTry bindings2 state evaluateBinding
                    |> Result.try
                        (\state2 -> evaluateExpression value state2
                            |> Result.try (\(value2, state3) -> Ok (value2, setLocals state3 originalLocals)))))

## Evaluate_binding function takes a state and a binding and returns a result after evaluating the binding.
evaluateBinding : State, Expr -> Result State Error
evaluateBinding = \state, binding ->
    expectList binding
    |> Result.try
        (\binding2 ->
            expect2 binding2
            |> Result.try
                (\(name, value) ->
                    expectAtom name
                    |> Result.try
                        (\name2 ->
                            evaluateExpression value state
                            |> Result.try
                                (\(value2, state2) ->
                                    Ok (insertLocal state2 name2 value2)))))

## Eq_builtin function takes a list of expressions and a state and returns a result after comparing the expressions.
eqBuiltin : List Expr, State -> Evaluated
eqBuiltin = \expressions, state ->
    expect2 expressions
    |> Result.try
        (\(a, b) ->
            evaluateExpression a state
            |> Result.try
                (\(a2, state2) ->
                    evaluateExpression b state2
                    |> Result.try
                        (\(b2, state3) ->
                            Ok (BoolExpr (compare a2 b2), state3))))

## Compare function takes two expressions and returns a boolean after comparing the expressions. It compares integers and booleans and returns true if they are equal. It compares lists by comparing each element of the list. It returns false for all other types of expressions.
compare : Expr, Expr -> Bool
compare = \a, b ->
    when (a, b) is
        (IntExpr ax, IntExpr bx) -> ax == bx
        (BoolExpr ax, BoolExpr bx) -> ax == bx
        (ListExpr ax, ListExpr bx) -> compareLists ax bx
        (_, _) -> Bool.false

## Compare_lists function takes two lists of expressions and returns a boolean fter comparing the lists.
compareLists : List Expr, List Expr -> Bool
compareLists = \a, b ->
    when (a, b) is
        ([], []) -> Bool.true
        ([x, .. as xs], [y, .. as ys]) -> (compare x y) && (compareLists xs ys)
        (_, _) -> Bool.false

## Not_builtin function takes a list of expressions and a state and returns a result after negating the expression.
notBuiltin : List Expr, State -> Evaluated
notBuiltin = \expressions, state ->
    expect1 expressions
    |> Result.try
        (\expression ->
            evaluateExpression expression state
            |> Result.try
                (\(value, state2) ->
                    expectBool value
                    |> Result.try
                        (\bool ->
                            Ok (BoolExpr (Bool.not bool), state2)
                        )
                )
        )

## And_builtin function takes a list of expressions and a state and returns a result after combining the expressions with the and operator. It returns true if all the expressions are true, otherwise it returns false.
andBuiltin : List Expr, State -> Evaluated
andBuiltin = \expressions, state ->
    when expressions is
        [] -> Ok (BoolExpr (Bool.true), state)
        [expression, .. as rest] ->
            evaluateExpression expression state
            |> Result.try (\(value, state2) -> expectBool value |> Result.try (\bool -> if bool then andBuiltin rest state2 else Ok (BoolExpr Bool.false, state2)))

## Or_builtin function takes a list of expressions and a state and returns a result after combining the expressions with the or operator. It returns true if any of the expressions are true, otherwise it returns false.
orBuiltin : List Expr, State -> Evaluated
orBuiltin = \expressions, state ->
    when expressions is
        [] -> Ok (BoolExpr Bool.false, state)
        [expression, .. as rest] ->
            evaluateExpression expression state
            |> Result.try \(value, state2) ->
                expectBool value
                |> Result.try (\bool -> if bool then Ok (BoolExpr Bool.true, state2) else orBuiltin rest state2)

## If_builtin function takes a list of expressions and a state and returns a result after evaluating the expressions.
## It evaluates the first expression if the condition is true, otherwise it evaluates the second expression.
ifBuiltin : List Expr, State -> Evaluated
ifBuiltin = \expressions, state ->
    expect3 expressions
    |> Result.try
        (
            \(condition, thenXyz, after) ->
                evaluateExpression condition state
                |> Result.try
                    (
                        \(condition2, state2) ->
                            expectBool condition2
                            |> Result.try
                                (
                                    \bool ->
                                        if bool then evaluateExpression thenXyz state2 else evaluateExpression after state2
                                )
                    )
        )
# -- Error handling --

## type_error function takes an expected type and a value and returns a result after creating a type error.
## It is used to create a type error when the expected type and the value do not match.
typeError : Str, Expr -> Error
typeError = \expected, value -> TypeError { expected, got: typeName value, value: display value }

## arity_error function takes an expected arity and a list of expressions and returns a result after creating an arity error. It is used to create an arity error when the expected arity and the length
arityError : U64, List _ -> Error
arityError = \expected, got -> IncorrectArity { expected, got: List.len got }

# -- Helpers --

## Expect_int function takes an expression and returns a result after checking if the expression is an integer. If not, it returns a type error.
expectInt : Expr -> Result I64 Error
expectInt = \value ->
    when value is
        IntExpr i -> Ok i
        _ -> Err (typeError "Int" value)

## Expect_atom function takes an expression and returns a result after checking if the expression is an atom. If not, it returns a type error.
expectAtom : Expr -> Result Str Error
expectAtom = \value ->
    when value is
        AtomExpr x -> Ok x
        _ -> Err (typeError "Atom" value)

## Expect_list function takes an expression and returns a result after checking if the expression is a list. If not, it returns a type error.
expectList : Expr -> Result (List Expr) Error
expectList = \value ->
    when value is
        ListExpr name -> Ok name
        _ -> Err (typeError "List" value)

## Expect_bool function takes an expression and returns a result after checking if the expression is a boolean. If not, it returns a type error.
expectBool : Expr -> Result Bool Error
expectBool = \value ->
    when value is
        BoolExpr x -> Ok x
        _ -> Err (typeError "Bool" value)

## Expect_1 function takes a list of expressions and returns a result after checking if the list contains exactly one expression. If not, it returns an arity error.
expect1 : List Expr -> Result Expr Error
expect1 = \expressions ->
    when expressions is
        [x] -> Ok x
        _ -> Err (arityError 1 expressions)

## Expect_2 function takes a list of expressions and returns a result after checking if the list contains exactly two expressions.
## If not, it returns an arity error.
expect2 : List Expr -> Result (Expr, Expr) Error
expect2 = \expressions ->
    when expressions is
        [x, y] -> Ok (x, y)
        _ -> Err (arityError 2 expressions)

## Expect_3 function takes a list of expressions and returns a result after checking if the list contains exactly three expressions.
## If not, it returns an arity error.
expect3 : List Expr -> Result (Expr, Expr, Expr) Error
expect3 = \expressions ->
    when expressions is
        [x, y, z] -> Ok (x, y, z)
        _ -> Err (arityError 3 expressions)

## Type_name function takes an expression and returns a string representing the type of the expression.
typeName : Expr -> Str
typeName = \value ->
    when value is
        IntExpr _ -> "Int"
        BoolExpr _ -> "Bool"
        ListExpr _ -> "List"
        ProcedureExpr _ -> "Procedure"
        ClosureExpr _ -> "Closure"
        AtomExpr _ -> "Atom"

## Print function takes an expression and prints a string representing the expression.
display : Expr -> Str
display = \value ->
    when value is
        IntExpr i -> Num.toStr i
        BoolExpr b -> if b then "true" else "false"
        ProcedureExpr _ -> "#<procedure>"
        ClosureExpr _ -> "#<closure>"
        AtomExpr x -> Str.withPrefix x "'"
        ListExpr elements ->
            elements
            |> List.map display
            |> Str.joinWith " "
            |> Str.withPrefix "'("
            |> Str.concat ")"

## no_code_test
expect
    out = eval ""
    out |> expectErr UnexpectedEndOfFile

## close_paren_test
expect
    out = eval "(+ 1 2))"
    out |> expectErr UnexpectedCloseParen

## empty_list_test
expect
    out = eval "()"
    out |> expectErr MissingProcedure

## int_test
expect
    out = eval "124"
    out |> expectOk "124"

## negative_int_test
expect
    out = eval "-124"
    out |> expectOk "-124"

## multiple_expressions_test
expect
    out = eval "1 2 3 4"
    out |> expectOk "4"

## add_two_test
expect
    out = eval "(+ 1 2)"
    out |> expectOk "3"

## add_many_test
expect
    out = eval "(+ 1 2 100 -7 -23)"
    out |> expectOk "73"

## add_nothing_test
expect
    out = eval "(+)"
    out |> expectOk "0"

## subtract_test
expect
    out = eval "(- 1 2 3)"
    out |> expectOk "-4"

## subtract_nothing_test
expect
    out = eval "(-)"
    out |> expectOk "0"

## multiply_test
expect
    out = eval "(* 1 2 3)"
    out |> expectOk "6"

## multiply_nothing_test
expect
    out = eval "(*)"
    out |> expectOk "1"

## divide_test
expect
    out = eval "(/ 20 2 5)"
    out |> expectOk "2"

## divide_nothing_test
expect
    out = eval "(/)"
    out |> expectOk "1"

## multiple_spaces_test
expect
    out = eval "  (    +     1   2   ) "
    out |> expectOk "3"

## nested_expressions_test
expect
    out = eval "(+ 1 2 (* 2 2))"
    out |> expectOk "7"

## def_test
expect
    out = eval "(define x 1) x"
    out |> expectOk "1"

## redefine_test
expect
    out = eval "(define x 1) (define x 2) x"
    out |> expectOk "2"

## var_in_expression_test
expect
    out = eval "(define x 1) (define x (+ x 2)) x"
    out |> expectOk "3"

# ## def_sequence_body_test
# expect
#     out = eval "(define x 1 2 3) x"
#     out |> expectErr (IncorrectArity { expected: 2, got: 4 })

## empty_test
expect
    out = eval "empty"
    out |> expectOk "'()"

## cons_1_test
expect
    out = eval "(cons 1 empty)"
    out |> expectOk "'(1)"

## cons_2_test
expect
    out = eval "(cons 1 (cons 2 empty))"
    out |> expectOk "'(1 2)"

## car_test
expect
    out = eval "(car (cons 1 (cons 2 empty)))"
    out |> expectOk "1"

## cdr_test
expect
    out = eval "(cdr (cons 1 (cons 2 empty)))"
    out |> expectOk "'(2)"

## false_test
expect
    out = eval "false"
    out |> expectOk "false"

## true_test
expect
    out = eval "true"
    out |> expectOk "true"

## not_true_test
expect
    out = eval "(not true)"
    out |> expectOk "false"

## not_false_test
expect
    out = eval "(not false)"
    out |> expectOk "true"

## and_empty_test
expect
    out = eval "(and)"
    out |> expectOk "true"

## and_true_test
expect
    out = eval "(and true true true true)"
    out |> expectOk "true"

## and_false_test
expect
    out = eval "(and true true true false)"
    out |> expectOk "false"

## or_empty_test
expect
    out = eval "(or)"
    out |> expectOk "false"

## or_true_test
expect
    out = eval "(or false false false true)"
    out |> expectOk "true"

## or_false_test
expect
    out = eval "(or false false false false)"
    out |> expectOk "false"

## eq_true_int_test
expect
    out = eval "(= 1 1)"
    out |> expectOk "true"

## eq_false_int_test
expect
    out = eval "(= 2 1)"
    out |> expectOk "false"

## eq_true_list_test
expect
    out = eval "(= (cons 1 empty) (cons 1 empty))"
    out |> expectOk "true"

## eq_false_list_test
expect
    out = eval "(= empty (cons 1 empty))"
    out |> expectOk "false"

## eq_proc_list_test
expect
    out = eval "(= cons cons)"
    out |> expectOk "false"

## eq_other_test
expect
    out = eval "(= 1 cons)"
    out |> expectOk "false"

## let_test
expect
    out = eval "(let ((x 1) (y (+ x 2))) y)"
    out |> expectOk "3"

## let_nested_test
expect
    # Here x is defined in both lets. The inner one does not leak
    out = eval "(let ((x 1) (y (let ((x 2)) x))) x)"
    out |> expectOk "1"

## procedure_printing_test
expect
    out = eval "let"
    out |> expectOk "#<procedure>"

## if_true_test
expect
    out = eval "(if (= 1 1) (+ 1 1) (+ 2 2))"
    out |> expectOk "2"

## if_false_test
expect
    out = eval "(if (= 1 2) (+ 1 1) (+ 2 2))"
    out |> expectOk "4"

## closure_test
expect
    out = eval "(define id (lambda (x) x)) (id 1)"
    out |> expectOk "1"

## closure_closes_over_scope_at_time_of_definition_test
expect
    out = eval "(define fn (let ((x 1) (return-x (lambda () x))) return-x)) (let ((x 2)) (fn))"
    out |> expectOk "1"
