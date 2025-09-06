module Nix.Op exposing
    ( equal, notEqual, and, or
    , append, cons
    , plus, minus, multiply, divide, power
    , lt, gt, lte, gte
    , pipe, pipeLeft
    , parens
    , update
    )

{-| This module helps generate operators!

So, this

    Nix.Op.equal (Nix.bool True) (Nix.bool False)

Would generate

    True == False


## Equality

@docs equal, notEqual, and, or


## Lists and strings

@docs append, cons


## Math

@docs plus, minus, multiply, divide, intDivide, power


## Comparisons

@docs lt, gt, lte, gte

@docs pipe, pipeLeft

@docs parens


## Parsing

@docs keep, skip


## Url parsing

@docs slash, query

-}

import Internal.Compiler as Compiler
import Nix exposing (Expression)
import Nix.Syntax.Expression as Exp
import Nix.Syntax.Node exposing (Node(..))



{- Infix operators!

   The goal is to make the following work


       one
           |> Nix.Op.or two
           |> Nix.Op.or three


       Nix.Op.or one two




   We're not really worried about allowing operators to be partially applied in a way that results in the following code.

       (<=) 5

   I mean, come on, we're generating code.  Let's make it clearer.


   We're also not worried about recreating infix notation in this lib.  So no need to do:

       applyBinOp (int 2) plus (int 3)




-}


{-| `==`
-}
equal : Expression -> Expression -> Expression
equal =
    applyInfix "=="


{-| `/=`
-}
notEqual : Expression -> Expression -> Expression
notEqual =
    applyInfix "/="


{-| `<`
-}
lt : Expression -> Expression -> Expression
lt =
    applyInfix "<"


{-| `>`
-}
gt : Expression -> Expression -> Expression
gt =
    applyInfix ">"


{-| `<=`
-}
lte : Expression -> Expression -> Expression
lte =
    applyInfix "<="


{-| `>=`
-}
gte : Expression -> Expression -> Expression
gte =
    applyInfix ">="


{-| `&&`
-}
and : Expression -> Expression -> Expression
and =
    applyInfix "&&"


{-| `||`
-}
or : Expression -> Expression -> Expression
or =
    applyInfix "||"


{-| The to-the-power-of operator `^`
-}
power : Expression -> Expression -> Expression
power =
    applyWithoutParens "^"


{-| `*`
-}
multiply : Expression -> Expression -> Expression
multiply =
    applyWithoutParens "*"


{-| `/`
-}
divide : Expression -> Expression -> Expression
divide =
    applyInfix "/"


{-| `//`
-}
update : Expression -> Expression -> Expression
update =
    applyInfix "//"


{-| `+`
-}
plus : Expression -> Expression -> Expression
plus =
    applyWithoutParens "+"


{-| `-`
-}
minus : Expression -> Expression -> Expression
minus =
    applyWithoutParens "-"


{-| `::`
-}
cons : Expression -> Expression -> Expression
cons =
    applyInfix "::"


{-| `++`
-}
append : Expression -> Expression -> Expression
append =
    applyInfix "++"


{-| `|>`

    Nix.value
        { importFrom = []
        , name = "thang"
        , annotation = Nothing
        }
        |> Nix.Op.pipe (Nix.value "thang2")
        |> Nix.Op.pipe (Nix.value "thang3")

Results in

    thang
        |> thang2
        |> thang3

-}
pipe : Expression -> Expression -> Expression
pipe r l =
    applyWithoutParens "|>" l r


{-| `<|`
-}
pipeLeft : Expression -> Expression -> Expression
pipeLeft =
    applyWithoutParens "<|"


{-| Wrap an expression in parentheses.

Generally you won't need this as `elm-codegen` handles parens for you, but it can be useful to semantically group operations from this module.

-}
parens : Expression -> Expression
parens (Compiler.Expression toExp) =
    Compiler.Expression
        (\index ->
            Compiler.parens (toExp index)
        )


applyInfix : String -> Expression -> Expression -> Expression
applyInfix symbol l r =
    Compiler.Expression <|
        \index ->
            let
                ( leftIndex, left ) =
                    Compiler.toExpressionDetails index l

                ( _, right ) =
                    Compiler.toExpressionDetails leftIndex r
            in
            Exp.OperatorApplicationExpr
                (Compiler.nodify (Compiler.parens left))
                (Compiler.nodify symbol)
                (Compiler.nodify (Compiler.parens right))


applyWithoutParens : String -> Expression -> Expression -> Expression
applyWithoutParens symbol l r =
    Compiler.Expression <|
        \index ->
            let
                ( leftIndex, left ) =
                    Compiler.toExpressionDetails index l

                ( _, right ) =
                    Compiler.toExpressionDetails leftIndex r
            in
            Exp.OperatorApplicationExpr
                (Compiler.nodify left)
                (Compiler.nodify symbol)
                (Compiler.nodify right)
