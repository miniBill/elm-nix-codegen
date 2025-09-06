module Nix exposing
    ( Expression, toString
    , bool, int, float, string
    , list, path, null
    , record, get
    , ifThen
    , fn, functionReduced, Arg
    , parse
    , apply, val
    )

{-|


## Basics

@docs Expression, toString

@docs bool, int, float, string

@docs list, path, null


## Records

@docs record, get


## Flow control

@docs ifThen

**Note** If you need `let` or `case` expressions, check out the docs for [`Elm.Let`](./Elm-Let) or [`Elm.Case`](./Elm-Case)!


## Functions

@docs fn, functionReduced, Arg


# Parsing existing Elm

@docs parse


# Low-level

@docs apply, val

-}

import Internal.Arg
import Internal.Compiler as Compiler
import Internal.Index as Index
import Internal.Write
import Nix.Parser
import Nix.Syntax.Expression as Exp exposing (Attribute(..), Name(..), Pattern(..), StringElement(..))
import Nix.Syntax.Node as Node exposing (Node(..))


{-| -}
type alias Expression =
    Compiler.Expression


{-| See what code this expression would generate!

**Note** - Check out the `Elm.ToString` module if this doesn't quite meet your needs!

-}
toString : Expression -> String
toString (Compiler.Expression toExp) =
    let
        expresh =
            toExp (Index.startIndex Nothing)
    in
    Internal.Write.writeExpression expresh


{-| -}
val : String -> Expression
val name =
    Compiler.Expression <|
        \_ ->
            -- This *must* be an un-protected name, where we only use
            -- literally what the dev gives us, because we are trying
            -- to refer to something that already exists.
            Exp.VariableExpr name


{-| -}
null : Expression
null =
    Compiler.Expression <|
        \_ ->
            Exp.NullExpr


{-| -}
bool : Bool -> Expression
bool on =
    Compiler.Expression <|
        \_ ->
            Exp.BoolExpr on


{-| -}
int : Int -> Expression
int intVal =
    Compiler.Expression <|
        \_ ->
            Exp.IntExpr intVal


{-| -}
float : Float -> Expression
float floatVal =
    Compiler.Expression <|
        \_ ->
            Exp.FloatExpr floatVal


{-| -}
string : String -> Expression
string literal =
    Compiler.Expression <|
        \_ ->
            Exp.StringExpr [ Exp.StringLiteral literal ]


path : String -> Expression
path literal =
    Compiler.Expression <|
        \_ ->
            literal
                |> String.split "/"
                |> List.map (\piece -> [ StringLiteral piece ])
                |> Exp.PathExpr


{-| -}
list : List Expression -> Expression
list exprs =
    Compiler.expression <|
        \index ->
            let
                exprDetails =
                    Compiler.thread index exprs
            in
            Exp.ListExpr (List.map Compiler.nodify exprDetails)


{-|

    Elm.record
        [ ( "name", Elm.string "Elm" )
        , ( "designation", Elm.string "Pretty fabulous" )
        ]

-}
record : List ( String, Expression ) -> Expression
record fields =
    Compiler.expression <|
        \index ->
            let
                unified :
                    { index : Index.Index
                    , fields : List Attribute
                    }
                unified =
                    fields
                        |> List.foldl
                            (\( fieldName, fieldExpression ) found ->
                                let
                                    ( newIndex, exp ) =
                                        Compiler.toExpressionDetails found.index fieldExpression
                                in
                                { index = newIndex
                                , fields =
                                    Attribute
                                        (Compiler.nodify [ Compiler.nodify (IdentifierName fieldName) ])
                                        (Compiler.nodify exp)
                                        :: found.fields
                                }
                            )
                            { fields = []
                            , index = index
                            }
            in
            unified.fields
                |> List.reverse
                |> Compiler.nodifyAll
                |> Exp.AttrSetExpr


{-|

    ifThen (Elm.bool True)
        (Elm.string "yes")
        (Elm.string "no")

Will generate

    if True then
        "yes"

    else
        "no"

If you need more than one branch, then chain them together!

     Elm.ifThen (Elm.bool True)
        (Elm.string "yes")
        (Elm.ifThen (Elm.bool True)
            (Elm.string "maybe")
            (Elm.string "no")
        )

Will generate

    if True then
        "yes"

    else if True then
        "maybe"

    else
        "no"

-}
ifThen : Expression -> Expression -> Expression -> Expression
ifThen condition thenBranch elseBranch =
    Compiler.Expression <|
        \index ->
            let
                ( condIndex, cond ) =
                    Compiler.toExpressionDetails index condition

                ( thenIndex, thenB ) =
                    Compiler.toExpressionDetails condIndex thenBranch

                ( _, elseB ) =
                    Compiler.toExpressionDetails thenIndex elseBranch
            in
            Exp.IfExpr
                (Compiler.nodify cond)
                (Compiler.nodify thenB)
                (Compiler.nodify elseB)


{-|

    record
        |> Elm.get "field"

results in

    record.field

-}
get : String -> Expression -> Expression
get fieldName recordExpression =
    Compiler.Expression <|
        \index ->
            let
                ( _, expr ) =
                    Compiler.toExpressionDetails index recordExpression
            in
            Exp.DotExpr
                (Compiler.nodify expr)
                [ Compiler.nodify (IdentifierName fieldName) ]
                Nothing


{-| -}
apply : Expression -> List Expression -> Expression
apply fnExp argExpressions =
    Compiler.expression
        (\index ->
            let
                ( annotationIndex, fnDetails ) =
                    Compiler.toExpressionDetails index fnExp

                args =
                    Compiler.thread annotationIndex argExpressions
            in
            Exp.ApplicationExpr
                (Compiler.nodify fnDetails)
                (Compiler.nodifyAll
                    (List.map Compiler.parens args)
                )
        )


{-| Create a function with a single argument.

This may seem a little weird the first time you encounter it, so let's break it down.

Here's what's happening for the `fn*` functions —

  - The `String` arguments are the **names of the arguments** for the generated function.
  - The attached `Maybe Annotation` is the type annotation. If you provide `Nothing`, then `elm-codegen` will infer the type for you!
  - The `(Expression -> Expression)` function is where we're providing you an `Expression` that represents an argument coming in to the generated function.

So, this

    Elm.fn (Elm.Arg.var "firstInt")
        (\firstArgument ->
            Elm.Op.plus
                (Elm.int 42)
                firstArgument
        )

Generates

    \firstInt -> 42 + firstInt

If you want to generate a **top level** function instead of an anonymous function, use `Elm.declaration`.

    Elm.declaration "add42" <|
        Elm.fn (Elm.Arg.var "firstInt")
            (\firstArgument ->
                Elm.Op.plus
                    (Elm.int 42)
                    firstArgument
            )

Results in

    add42 : Int -> Int
    add42 firstInt =
        42 + firstInt

**Note** — Elm CodeGen will protect variable names if they're used in a nested `fn*` by adding a string of numbers to the end of the name. So, you may see a variable name be something like `myVariable_0_1`.

-}
fn : Arg arg -> (arg -> Expression) -> Expression
fn arg1 toExpression =
    Compiler.Expression
        (\index ->
            let
                argDetails : { pattern : Node Pattern, value : arg, index : Index.Index }
                argDetails =
                    Internal.Arg.toDetails index arg1

                ( _, return ) =
                    Compiler.toExpressionDetails (Index.next index)
                        (toExpression argDetails.value)
            in
            Exp.FunctionExpr argDetails.pattern (Compiler.nodify return)
        )


{-| Check out the `Elm.Arg` module for more information on how to create one of these.
-}
type alias Arg val =
    Internal.Arg.Arg val


{-| This is a special case of function declaration which will _reduce_ itself if possible.

Meaning, if this would generate the following code

    \myArg -> someOtherFunction myArg

Then it will replace itself with just

    someOtherFunction

**Note** you likely won't need this! It's generally used by the package-helper generator, but that might be a relatively special case.

-}
functionReduced : String -> (Expression -> Expression) -> Expression
functionReduced argBaseName toExpression =
    Compiler.expression <|
        \index ->
            let
                ( arg1Name, newIndex ) =
                    Index.getName argBaseName index

                arg1 : Expression
                arg1 =
                    val arg1Name

                (Compiler.Expression toExpr) =
                    toExpression arg1

                return =
                    toExpr newIndex
            in
            betaReduce <|
                Exp.FunctionExpr
                    (Compiler.nodify (VarPattern arg1Name))
                    (Compiler.nodify return)


betaReduce : Exp.Expression -> Exp.Expression
betaReduce e =
    let
        extractLastArg : Exp.Expression -> Maybe String
        extractLastArg argExpression =
            case argExpression of
                Exp.VariableExpr n ->
                    Just n

                Exp.ParenthesizedExpr (Node _ p) ->
                    extractLastArg p

                _ ->
                    Nothing
    in
    case e of
        Exp.FunctionExpr (Node _ (VarPattern lastLambdaArg)) (Node _ (Exp.ApplicationExpr f applicationArgs)) ->
            case List.reverse applicationArgs of
                [] ->
                    if extractLastArg (Node.value f) == Just lastLambdaArg then
                        Exp.VariableExpr "identity"

                    else
                        e

                (Node _ last) :: tail ->
                    let
                        otherArgs : List (Node Exp.Expression)
                        otherArgs =
                            List.reverse tail
                    in
                    if extractLastArg last == Just lastLambdaArg then
                        case otherArgs of
                            [] ->
                                betaReduce (Node.value f)

                            _ ->
                                Exp.ApplicationExpr f otherArgs

                    else
                        e

        _ ->
            e


{-| -}
parse : String -> Result String Expression
parse source =
    case Nix.Parser.parse source of
        Err _ ->
            Err "Uh oh"

        Ok (Node _ parsedFile) ->
            Ok (Compiler.expression (\_ -> parsedFile))
