module Internal.Compiler exposing
    ( Expression(..)
    , Restrictions(..)
    , Visited
    , Warning
    , denode
    , denodeAll
    , denodeMaybe
    , expression
    , nodeAtLine
    , nodify
    , nodifyAll
    , parens
    , thread
    , toExpressionDetails
    , toVar
    )

import Internal.Index as Index exposing (Index)
import Nix.Syntax.Expression as Exp
import Nix.Syntax.Node as Node exposing (Node(..))
import Set exposing (Set)


type alias Warning =
    { declaration : String
    , warning : String
    }


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


toVar :
    Index
    -> String
    ->
        { name : String
        , typename : String
        , val : Expression
        , index : Index
        }
toVar index desiredName =
    let
        ( name, newIndex ) =
            Index.getName desiredName index

        typename : String
        typename =
            Index.protectTypeName desiredName index
    in
    { name = name
    , typename = typename
    , index = newIndex
    , val =
        Expression <| \_ -> Exp.VariableExpr name
    }


denode : Node a -> a
denode =
    Node.value


denodeAll : List (Node a) -> List a
denodeAll =
    List.map denode


denodeMaybe : Maybe (Node a) -> Maybe a
denodeMaybe =
    Maybe.map denode


nodify : a -> Node a
nodify exp =
    Node.empty exp


nodeAtLine : Int -> a -> Node a
nodeAtLine line exp =
    Node
        { start = { column = 0, row = line }
        , end = { column = 0, row = line }
        }
        exp


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


type alias Visited =
    Set String


type Restrictions
    = NoRestrictions
    | IsNumber
    | IsAppendable
    | IsComparable
    | IsAppendableComparable
    | Overconstrainted (List Restrictions)
