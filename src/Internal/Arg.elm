module Internal.Arg exposing
    ( Arg(..), Expression, toDetails
    , aliasAs
    , var
    , record, field
    , ignore
    , fieldWithDefault
    )

{-|

@docs Arg, Expression, toDetails

@docs aliasAs

@docs var

@docs record, field

@docs ignore

-}

import Internal.Compiler as Compiler
import Internal.Format as Format
import Internal.Index as Index
import Nix.Syntax.Expression as Exp exposing (AttributePattern(..), Expression(..), Pattern(..))
import Nix.Syntax.Node as Node


type alias Expression =
    Compiler.Expression


type Arg val
    = Arg
        (Index.Index
         ->
            { pattern : Node.Node Pattern
            , value : val
            , index : Index.Index
            }
        )


toDetails :
    Index.Index
    -> Arg val
    ->
        { pattern : Node.Node Pattern
        , value : val
        , index : Index.Index
        }
toDetails index (Arg arg) =
    arg index


{-| -}
aliasAs : String -> Arg arg -> Arg ( arg, Expression )
aliasAs aliasName (Arg toArgDetails) =
    Arg
        (\index ->
            let
                innerArgDetails =
                    toArgDetails index
            in
            { pattern =
                AtPattern
                    innerArgDetails.pattern
                    (Compiler.nodify aliasName)
                    |> Compiler.nodify
            , index = Index.next innerArgDetails.index
            , value =
                ( innerArgDetails.value
                , val innerArgDetails.index aliasName
                )
            }
        )


{-| -}
ignore : Arg Expression
ignore =
    Arg
        (\index ->
            { pattern = Compiler.nodify AllPattern
            , index = Index.next index
            , value =
                Compiler.Expression <|
                    \_ ->
                        NullExpr
            }
        )


val :
    Index.Index
    -> String
    -> Compiler.Expression
val _ name =
    Compiler.Expression <|
        \_ ->
            -- This *must* be an un-protected name, where we only use
            -- literally what the dev gives us, because we are trying
            -- to refer to something that already exists.
            Exp.VariableExpr (Format.sanitize name)


{-| -}
var : String -> Arg Expression
var rawName =
    Arg
        (\index ->
            let
                ( name, nameIndex ) =
                    Index.getName rawName index
            in
            { pattern = Compiler.nodify (VarPattern name)
            , index = Index.next nameIndex
            , value = val nameIndex name
            }
        )


record : a -> { open : Bool } -> Arg a
record toRecord { open } =
    Arg
        (\index ->
            { pattern =
                Compiler.nodify (AttrSetPattern [] { open = open })
            , index = Index.next index
            , value =
                toRecord
            }
        )


{-| -}
field : String -> Arg (Expression -> a) -> Arg a
field name (Arg arg) =
    Arg
        (\index ->
            let
                toRecord =
                    arg index

                toAdd : AttributePattern
                toAdd =
                    AttributePattern (Compiler.nodify name) Nothing
            in
            { pattern =
                case Compiler.denode toRecord.pattern of
                    AttrSetPattern fields open ->
                        Compiler.nodify (AttrSetPattern (fields ++ [ toAdd ]) open)

                    _ ->
                        Compiler.nodify (AttrSetPattern [ toAdd ] { open = False })
            , index = Index.next index
            , value = toRecord.value (val index name)
            }
        )


{-| -}
fieldWithDefault : String -> Expression -> Arg (Expression -> a) -> Arg a
fieldWithDefault name default (Arg arg) =
    Arg
        (\index ->
            let
                toRecord =
                    arg defaultIndex

                ( defaultIndex, defaultDetails ) =
                    Compiler.toExpressionDetails index default

                toAdd : AttributePattern
                toAdd =
                    AttributePattern (Compiler.nodify name) (Just (Compiler.nodify defaultDetails))
            in
            { pattern =
                case Compiler.denode toRecord.pattern of
                    AttrSetPattern fields open ->
                        Compiler.nodify (AttrSetPattern (fields ++ [ toAdd ]) open)

                    _ ->
                        Compiler.nodify (AttrSetPattern [ toAdd ] { open = False })
            , index = Index.next index
            , value = toRecord.value (val index name)
            }
        )
