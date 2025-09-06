module Nix.Let exposing
    ( letIn, value, Let
    , fn
    , toExpression, withBody
    )

{-| This module is for building `let` expressions.

@docs letIn, value, Let

Here's a brief example to get you started

    import Nix
    import Nix.Let as Let

    Let.letIn
        (\one two ->
            Nix.Op.append one two
        )
        |> Let.value "one" (Nix.string "Hello")
        |> Let.value "two" (Nix.string "World!")
        |> Let.toExpression

Will translate into

    let
        one =
            "Hello!"

        two =
            "World"
    in
    one ++ two


# Functions

Here's an example of declaring functions in a let expression:

    import Nix
    import Nix.Let as Let

    Let.letIn
        (\myFn ->
            myFn (Nix.bool True)
        )
        |> Let.fn "myFn"
            (Arg.varWith "arg" Type.bool )
            (\arg ->
                Nix.ifThen arg
                    (Nix.string "True")
                    (Nix.string "False")
            )
        |> Let.toExpression

will generate

    let
        myFn arg =
            if arg then
                "True"

            else
                "False"
    in
    myFn True

@docs fn


# Converting to an Expression

@docs toExpression, withBody

-}

import Internal.Arg
import Internal.Compiler as Compiler
import Internal.Index as Index
import Nix exposing (Expression)
import Nix.Syntax.Expression as Exp exposing (Name(..))
import Nix.Syntax.Node as Node


{-| -}
type Let a
    = Let
        (Index.Index
         ->
            { letDecls : List (Node.Node Exp.LetDeclaration)
            , index : Index.Index
            , return : a
            }
        )


{-| -}
letIn : a -> Let a
letIn return =
    Let
        (\index ->
            { letDecls = []
            , index = index
            , return = return
            }
        )


with : Let a -> Let (a -> b) -> Let b
with (Let toScopeA) (Let toScopeAB) =
    Let
        (\index ->
            let
                resultA : { letDecls : List (Node.Node Exp.LetDeclaration), index : Index.Index, return : a }
                resultA =
                    toScopeA index

                resultB : { letDecls : List (Node.Node Exp.LetDeclaration), index : Index.Index, return : a -> b }
                resultB =
                    toScopeAB resultA.index
            in
            { letDecls = resultA.letDecls ++ resultB.letDecls
            , index = resultB.index
            , return = resultB.return resultA.return
            }
        )


{-| -}
value : String -> Expression -> Let (Expression -> a) -> Let a
value desiredName valueExpr sourceLet =
    with
        (Let
            (\index ->
                let
                    ( name, secondIndex ) =
                        Index.getName desiredName index

                    ( finalIndex, details ) =
                        Compiler.toExpressionDetails secondIndex valueExpr
                in
                { letDecls =
                    [ Compiler.nodify <|
                        Exp.LetDeclaration
                            (Compiler.nodify [ Compiler.nodify (IdentifierName name) ])
                            (Compiler.nodify details)
                    ]
                , index = finalIndex
                , return =
                    Compiler.Expression
                        (\_ ->
                            Exp.VariableExpr name
                        )
                }
            )
        )
        sourceLet


{-| -}
fn :
    String
    -> Nix.Arg arg
    -> (arg -> Expression)
    -> Let ((Expression -> Expression) -> a)
    -> Let a
fn desiredName arg toInnerFn sourceLet =
    sourceLet
        |> with
            (Let
                (\index ->
                    let
                        ( name, secondIndex ) =
                            Index.getName desiredName index

                        argDetails =
                            Internal.Arg.toDetails secondIndex arg

                        ( finalIndex, innerFnDetails ) =
                            Compiler.toExpressionDetails
                                argDetails.index
                                (toInnerFn argDetails.value)
                    in
                    { letDecls =
                        [ Compiler.nodify
                            (Exp.LetDeclaration
                                (Compiler.nodify [ Compiler.nodify (IdentifierName name) ])
                                (Compiler.nodify
                                    (Exp.FunctionExpr argDetails.pattern
                                        (Compiler.nodify innerFnDetails)
                                    )
                                )
                            )
                        ]
                    , index = finalIndex
                    , return =
                        \callerArg ->
                            Nix.apply
                                (Compiler.Expression (\_ -> Exp.VariableExpr name))
                                [ callerArg
                                ]
                    }
                )
            )


{-| -}
toExpression : Let Expression -> Expression
toExpression (Let toScope) =
    Compiler.Expression <|
        \index ->
            let
                scope : { letDecls : List (Node.Node Exp.LetDeclaration), index : Index.Index, return : Expression }
                scope =
                    toScope index

                ( _, return ) =
                    Compiler.toExpressionDetails scope.index scope.return
            in
            -- if we're leading into another let expression, just merge with it.
            case return of
                Exp.LetExpr innerDecls innerReturn ->
                    Exp.LetExpr
                        (List.reverse scope.letDecls
                            ++ innerDecls
                        )
                        innerReturn

                _ ->
                    Exp.LetExpr
                        (List.reverse scope.letDecls)
                        (Compiler.nodify return)


{-| Define the body of your `let` at the bottom instead of the top so it matches the generated syntax a bit closer.

These two are equivalent
import Nix
import Nix.Let as Let

      Let.letIn
          (\one two ->
              Nix.Op.append one two
          )
          |> Let.value "one" (Nix.string "Hello")
          |> Let.value "two" (Nix.string "World!")
          |> Let.toExpression


      Let.letIn Tuple.pair
          |> Let.value "one" (Nix.string "Hello")
          |> Let.value "two" (Nix.string "World!")
          |> Let.withBody
              (\(one, two) ->
                  Nix.Op.append one two
              )

And will generate

      let
          one = "Hello"
          two = "World!"
      in
      one ++ two

-}
withBody : (val -> Expression) -> Let val -> Expression
withBody toBody (Let toScope) =
    Compiler.Expression <|
        \index ->
            let
                letDetails :
                    { letDecls : List (Node.Node Exp.LetDeclaration)
                    , index : Index.Index
                    , return : val
                    }
                letDetails =
                    toScope index

                ( _, return ) =
                    letDetails.return
                        |> toBody
                        |> Compiler.toExpressionDetails letDetails.index
            in
            -- if we're leading into another let expression, just merge with it.
            case return of
                Exp.LetExpr innerDecls innerReturn ->
                    Exp.LetExpr
                        (List.reverse letDetails.letDecls
                            ++ innerDecls
                        )
                        innerReturn

                _ ->
                    Exp.LetExpr
                        (List.reverse letDetails.letDecls)
                        (Compiler.nodify return)
