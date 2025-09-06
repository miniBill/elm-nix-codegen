module Nix.Arg exposing
    ( var
    , record, field, fieldWithDefault
    , aliasAs
    , ignore
    )

{-| An `Arg` can be used to pattern match on the arguments of a function.

    let
        args =
            Nix.Arg.tuple (Arg.var "first") (Arg.var "second")
    in
    Nix.fn args
        (\( first, second ) ->
            Nix.record
                [ ( "first", first )
                , ( "second", second )
                ]
        )

Will generate

    \( first, second ) ->
        { first = first
        , second = second
        }

Or they can be used to unpack values in the branch of a case expression.

    Nix.Case.custom (Nix.val "myVar") (Nix.Annotation.named [] "MyCustomType)
        [ Nix.branch "MyCustomType" (Arg.tuple (Arg.var "first") (Arg.var "second"))
            (\( first, second ) ->
                Nix.record
                    [ ( "first", first )
                    , ( "second", second )
                    ]
            )
        ]

Will generate

      case myVar of
          MyCustomType first second ->
              { first = first
              , second = second
              }

@docs var

@docs record, field, fieldWithDefault

@docs aliasAs

@docs ignore

-}

import Internal.Arg
import Nix exposing (Arg, Expression)


{-| -}
var : String -> Arg Expression
var =
    Internal.Arg.var


{-| Unpack a pattern, but keep a reference to the original value.

    let
        args =
            Nix.Arg.customType "MyCustomType" Tuple.pair
                |> Nix.Arg.item (Arg.var "first")
                |> Nix.Arg.item (Arg.var "second")
                |> Nix.Arg.aliasAs "myAlias"
    in
    Nix.fn args
        (\( ( first, second ), myAlias ) ->
            Nix.record
                [ ( "first", first )
                , ( "second", second )
                , ( "myAlias", myAlias )
                ]
        )

Will generate

    \((MyCustomType first second) as myAlias) ->
        { first = first
        , second = second
        , myAlias = myAlias
        }

-}
aliasAs : String -> Arg arg -> Arg ( arg, Expression )
aliasAs =
    Internal.Arg.aliasAs


{-| Unpack record fields.

    let
        args =
            Nix.Arg.record
                |> Nix.Arg.field "first" (Arg.var "first")
                |> Nix.Arg.field "second" (Arg.var "second")
    in
    Nix.fn args
        (\{ first, second } ->
            Nix.record
                [ ( "first", first )
                , ( "second", second )
                ]
        )

Would generate

    \{ first, second } ->
        { first = first
        , second = second
        }

-}
record : fields -> { open : Bool } -> Arg fields
record =
    Internal.Arg.record


{-| -}
field : String -> Arg (Expression -> a) -> Arg a
field =
    Internal.Arg.field


{-| -}
fieldWithDefault : String -> Expression -> Arg (Expression -> a) -> Arg a
fieldWithDefault =
    Internal.Arg.fieldWithDefault


{-| Will generate `_` to ignore an argument or pattern.
-}
ignore : Arg Expression
ignore =
    Internal.Arg.ignore
