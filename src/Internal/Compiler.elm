module Internal.Compiler exposing
    ( Expression(..)
    , Restrictions(..)
    , denode
    , denodeAll
    , expression
    , nodify
    , nodifyAll
    , parens
    , thread
    , toExpressionDetails
    )

import Internal.Index as Index exposing (Index)
import Nix.Syntax.Expression as Exp
import Nix.Syntax.Node as Node exposing (Node)


{-| -}
type Expression
    = Expression (Index -> Exp.Expression)


{-| An expression should always call `Index.dive`.

This helper does that.

-}
expression : (Index -> Exp.Expression) -> Expression
expression toExp =
    Expression
        (\index ->
            toExp (Index.dive index)
        )


{-| -}
parens : Exp.Expression -> Exp.Expression
parens expr =
    case expr of
        Exp.NullExpr ->
            expr

        Exp.BoolExpr _ ->
            expr

        Exp.IntExpr _ ->
            expr

        Exp.StringExpr _ ->
            expr

        Exp.FloatExpr _ ->
            expr

        Exp.ParenthesizedExpr _ ->
            expr

        Exp.ListExpr _ ->
            expr

        Exp.VariableExpr _ ->
            expr

        Exp.AttrSetExpr _ ->
            expr

        Exp.FunctionExpr _ _ ->
            expr

        Exp.LookupPathExpr _ ->
            expr

        Exp.DotExpr _ _ _ ->
            expr

        Exp.PathExpr _ ->
            expr

        _ ->
            Exp.ParenthesizedExpr (nodify expr)


denode : Node a -> a
denode =
    Node.value


denodeAll : List (Node a) -> List a
denodeAll =
    List.map denode


nodify : a -> Node a
nodify exp =
    Node.empty exp


nodifyAll : List a -> List (Node a)
nodifyAll =
    List.map nodify


toExpressionDetails : Index -> Expression -> ( Index, Exp.Expression )
toExpressionDetails index (Expression toExp) =
    ( Index.next index, toExp index )


thread : Index -> List Expression -> List Exp.Expression
thread index exps =
    threadHelper index exps []


threadHelper : Index -> List Expression -> List Exp.Expression -> List Exp.Expression
threadHelper index exps rendered =
    case exps of
        [] ->
            List.reverse rendered

        (Expression toExpDetails) :: remain ->
            threadHelper (Index.next index)
                remain
                (toExpDetails index :: rendered)


type Restrictions
    = Overconstrainted (List Restrictions)
