module Internal.Write exposing (writeExpression)

{-| This is borrowed basically in it's entirety from: <https://github.com/the-sett/elm-syntax-dsl/blob/master/src/Elm/Pretty.elm>

Thank you Rupert!

-}

import Internal.Compiler exposing (denodeAll, nodify)
import Nix.Syntax.Expression exposing (AttrPath, Attribute(..), AttributePattern(..), Expression(..), LetDeclaration(..), Name(..), Pattern(..), StringElement(..))
import Nix.Syntax.Infix exposing (InfixDirection(..))
import Nix.Syntax.Node exposing (Node(..))
import Pretty exposing (Doc)


writeExpression : Expression -> String
writeExpression exp =
    prettyExpression exp
        |> Pretty.pretty 80



--== Patterns


adjustPatternParentheses : Bool -> Pattern -> Pattern
adjustPatternParentheses isTop pattern =
    let
        addParens : Pattern -> Pattern
        addParens pat =
            case ( isTop, pat ) of
                ( False, AtPattern _ _ ) ->
                    nodify pat |> ParenthesizedPattern

                ( False, ReverseAtPattern _ _ ) ->
                    nodify pat |> ParenthesizedPattern

                _ ->
                    pat

        removeParens : Pattern -> Pattern
        removeParens pat =
            case pat of
                ParenthesizedPattern (Node _ innerPat) ->
                    if shouldRemove innerPat then
                        removeParens innerPat

                    else
                        pat

                _ ->
                    pat

        shouldRemove : Pattern -> Bool
        shouldRemove pat =
            case ( isTop, pat ) of
                ( _, AtPattern _ _ ) ->
                    False

                ( _, ReverseAtPattern _ _ ) ->
                    False

                _ ->
                    isTop
    in
    removeParens pattern
        |> addParens


prettyPatternInner : Bool -> Pattern -> Doc t
prettyPatternInner isTop pattern =
    case adjustPatternParentheses isTop pattern of
        AllPattern ->
            Pretty.string "_"

        AttrSetPattern fields { open } ->
            let
                prettyFields =
                    if open then
                        List.map prettyAttributePattern fields ++ [ Pretty.string "..." ]

                    else
                        List.map prettyAttributePattern fields
            in
            prettyFields
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.surround Pretty.space Pretty.space
                |> Pretty.braces

        VarPattern var ->
            prettyIdentifier var

        AtPattern (Node _ pat) (Node _ name) ->
            [ prettyPatternInner False pat
            , Pretty.string "@"
            , Pretty.string name
            ]
                |> Pretty.words

        ReverseAtPattern (Node _ name) (Node _ pat) ->
            [ Pretty.string name
            , Pretty.string "@"
            , prettyPatternInner False pat
            ]
                |> Pretty.words

        ParenthesizedPattern (Node _ pat) ->
            prettyPatternInner True pat
                |> Pretty.parens


prettyAttributePattern : AttributePattern -> Doc t
prettyAttributePattern (AttributePattern (Node _ name) default) =
    case default of
        Nothing ->
            Pretty.string name

        Just (Node _ def) ->
            [ Pretty.string name
            , Pretty.string "?"
            , prettyExpression def
            ]
                |> Pretty.words



--== Expressions


type alias Context =
    { precedence : Int
    }


topContext : Context
topContext =
    { precedence = 0
    }


bottomContext : Context
bottomContext =
    { precedence = 11
    }


{-| Pretty prints an expression.
-}
prettyExpression : Expression -> Doc t
prettyExpression expression =
    prettyExpressionInner topContext 0 expression
        |> Tuple.first


prettyExpressionInner : Context -> Int -> Expression -> ( Doc t, Bool )
prettyExpressionInner context indent expression =
    let
        noninfix : ( Doc t, Bool ) -> ( Doc t, Bool )
        noninfix =
            showParen (context.precedence > 10)
    in
    case expression of
        ApplicationExpr h t ->
            noninfix <| prettyApplication indent (h :: t)

        OperatorApplicationExpr exprl (Node _ symbol) exprr ->
            showParen (context.precedence > precedence symbol) <|
                prettyOperatorApplication indent exprl symbol exprr

        VariableExpr val ->
            ( Pretty.string val
            , False
            )

        IfExpr exprBool exprTrue exprFalse ->
            noninfix <|
                prettyIfBlock indent exprBool exprTrue exprFalse

        IntExpr val ->
            ( Pretty.string (String.fromInt val)
            , False
            )

        FloatExpr val ->
            ( Pretty.string (String.fromFloat val)
            , False
            )

        NegationExpr (Node _ expr) ->
            noninfix <|
                let
                    ( prettyExpr, alwaysBreak ) =
                        prettyExpressionInner bottomContext 4 expr
                in
                ( Pretty.string "-"
                    |> Pretty.a prettyExpr
                , alwaysBreak
                )

        StringExpr val ->
            ( prettyString val
            , False
            )

        ParenthesizedExpr (Node _ expr) ->
            prettyExpressionInner context indent expr

        LetExpr letDeclarations letExpression ->
            noninfix <| prettyLetExpr indent letDeclarations letExpression

        FunctionExpr arg child ->
            noninfix <| prettyFunction indent arg child

        AttrSetExpr setters ->
            prettyAttrSet setters

        ListExpr exprs ->
            prettyList indent exprs

        DotExpr expr field default ->
            prettyRecordAccess expr field default

        NullExpr ->
            ( Pretty.string "null", False )

        BoolExpr b ->
            if b then
                ( Pretty.string "true", False )

            else
                ( Pretty.string "false", False )

        PathExpr components ->
            ( components
                |> List.map
                    (\g ->
                        g
                            |> List.map prettyStringElement
                            |> Pretty.join Pretty.empty
                    )
                |> Pretty.join slash
            , False
            )

        LookupPathExpr components ->
            ( components
                |> List.map Pretty.string
                |> Pretty.join (Pretty.string "/")
                |> Pretty.surround (Pretty.string "<") (Pretty.string ">")
            , False
            )

        WithExpr _ _ ->
            Debug.todo "branch 'WithExpr _ _' not implemented"

        AssertExpr _ _ ->
            Debug.todo "branch 'AssertExpr _ _' not implemented"

        HasAttributeExpr _ _ ->
            Debug.todo "branch 'HasAttributeExpr _ _' not implemented"


prettyApplication : Int -> List (Node Expression) -> ( Doc t, Bool )
prettyApplication indent exprs =
    let
        ( prettyExpressions, alwaysBreak ) =
            List.map
                (prettyExpressionInner bottomContext indent)
                (denodeAll exprs)
                |> List.unzip
                |> Tuple.mapSecond (List.any identity)
    in
    ( prettyExpressions
        |> Pretty.lines
        |> Pretty.hang indent
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyOperatorApplication : Int -> Node Expression -> String -> Node Expression -> ( Doc t, Bool )
prettyOperatorApplication indent (Node _ exprl) symbol (Node _ exprr) =
    let
        prec : Int
        prec =
            precedence symbol

        ( lprec, rprec ) =
            case direction symbol of
                Left ->
                    ( prec, prec + 1 )

                Non ->
                    ( prec + 1, prec + 1 )

                Right ->
                    ( prec + 1, prec )

        ( left, breakLeft ) =
            prettyExpressionInner { precedence = lprec } indent exprl

        ( right, breakRight ) =
            prettyExpressionInner { precedence = rprec } (indent + 4) exprr

        alwaysBreak : Bool
        alwaysBreak =
            breakLeft || breakRight
    in
    ( left
        |> Pretty.a Pretty.space
        |> Pretty.a (Pretty.string symbol)
        |> Pretty.a Pretty.space
        |> Pretty.a right
        |> Pretty.align
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyIfBlock : Int -> Node Expression -> Node Expression -> Node Expression -> ( Doc t, Bool )
prettyIfBlock indent exprBool exprTrue exprFalse =
    let
        innerIfBlock : Node Expression -> Node Expression -> Node Expression -> List (Doc t)
        innerIfBlock (Node _ innerExprBool) (Node _ innerExprTrue) (Node _ innerExprFalse) =
            let
                ifPart : Doc t
                ifPart =
                    let
                        ( _, alwaysBreak ) =
                            prettyExpressionInner topContext 2 innerExprBool
                    in
                    [ [ Pretty.string "if"
                      , prettyExpressionInner topContext 2 innerExprBool |> Tuple.first
                      ]
                        |> Pretty.lines
                        |> optionalGroup alwaysBreak
                        |> Pretty.nest indent
                    , Pretty.string "then"
                    ]
                        |> Pretty.lines
                        |> optionalGroup alwaysBreak

                truePart : Doc t
                truePart =
                    prettyExpressionInner topContext 2 innerExprTrue
                        |> Tuple.first
                        |> Pretty.indent indent

                elsePart : Doc t
                elsePart =
                    Pretty.line
                        |> Pretty.a (Pretty.string "else")

                falsePart : List (Doc t)
                falsePart =
                    case innerExprFalse of
                        IfExpr nestedExprBool nestedExprTrue nestedExprFalse ->
                            innerIfBlock nestedExprBool nestedExprTrue nestedExprFalse

                        _ ->
                            [ prettyExpressionInner topContext 2 innerExprFalse
                                |> Tuple.first
                                |> Pretty.indent indent
                            ]
            in
            case falsePart of
                [] ->
                    []

                [ falseExpr ] ->
                    [ ifPart
                    , truePart
                    , elsePart
                    , falseExpr
                    ]

                hd :: tl ->
                    List.append
                        [ ifPart
                        , truePart
                        , [ elsePart, hd ] |> Pretty.words
                        ]
                        tl

        prettyExpressions : List (Doc t)
        prettyExpressions =
            innerIfBlock exprBool exprTrue exprFalse
    in
    ( prettyExpressions
        |> Pretty.lines
        |> Pretty.align
    , True
    )


showParen : Bool -> ( Doc t, Bool ) -> ( Doc t, Bool )
showParen show ( child, alwaysBreak ) =
    if show then
        let
            open : Doc t
            open =
                Pretty.string "("

            close : Doc t
            close =
                Pretty.a (Pretty.string ")") Pretty.tightline
        in
        ( child
            |> Pretty.nest 1
            |> Pretty.surround open close
            |> Pretty.align
            |> optionalGroup alwaysBreak
        , alwaysBreak
        )

    else
        ( child, alwaysBreak )


prettyLetExpr : Int -> List (Node LetDeclaration) -> Node Expression -> ( Doc t, Bool )
prettyLetExpr indent declarations (Node _ expression) =
    ( [ Pretty.string "let"
      , denodeAll declarations
            |> List.map (prettyLetDeclaration indent)
            |> doubleLines
            |> Pretty.indent indent
      , Pretty.string "in"
      , prettyExpressionInner topContext 2 expression |> Tuple.first
      ]
        |> Pretty.lines
        |> Pretty.align
    , True
    )


prettyLetDeclaration : Int -> LetDeclaration -> Doc t
prettyLetDeclaration indent letDecl =
    case letDecl of
        LetDeclaration (Node _ path) (Node _ expr) ->
            [ prettyAttrPath path
            , Pretty.string "= "
            ]
                |> Pretty.words
                |> Pretty.a
                    (prettyExpression expr
                        |> Pretty.nest (2 + indent)
                    )

        LetInheritVariables _ ->
            Debug.todo "prettyLetDeclaration > LetInheritVariables _"

        LetInheritFromAttrSet _ _ ->
            Debug.todo "prettyLetDeclaration > LetInheritFromAttrSet _ _"


prettyAttrPath : AttrPath -> Doc t
prettyAttrPath names =
    names
        |> List.map (\(Node _ name) -> prettyName name)
        |> Pretty.join dot


prettyName : Name -> Doc t
prettyName name =
    case name of
        IdentifierName id ->
            prettyIdentifier id

        StringName elems ->
            prettyString elems

        InterpolationName _ ->
            Debug.todo "prettyName > InterpolationName _"


prettyIdentifier : String -> Doc t
prettyIdentifier id =
    if String.all (\c -> Char.isAlphaNum c || c == '-' || c == '_' || c == '.') id then
        Pretty.string id

    else
        Pretty.string id |> quotes


prettyString : List StringElement -> Doc t
prettyString elems =
    elems
        |> List.map prettyStringElement
        |> Pretty.join Pretty.empty
        |> quotes


prettyStringElement : StringElement -> Doc t
prettyStringElement elem =
    case elem of
        StringLiteral s ->
            Pretty.string (escape s)

        StringInterpolation (Node _ e) ->
            Pretty.string "${"
                |> Pretty.a (prettyExpression e)
                |> Pretty.a (Pretty.string "}")


prettyFunction : Int -> Node Pattern -> Node Expression -> ( Doc t, Bool )
prettyFunction indent (Node _ arg) (Node _ child) =
    let
        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner topContext 4 child
    in
    ( [ prettyPatternInner False arg
            |> Pretty.a (Pretty.string ": ")
      , prettyExpr
      ]
        |> Pretty.lines
        |> Pretty.hang indent
        |> optionalGroup alwaysBreak
    , alwaysBreak
    )


prettyAttrSet : List (Node Attribute) -> ( Doc t, Bool )
prettyAttrSet setters =
    case setters of
        [] ->
            ( Pretty.string "{}", False )

        _ ->
            let
                ( prettyAttributes, alwaysBreak ) =
                    setters
                        |> List.map (\(Node _ attr) -> prettyAttribute attr)
                        |> List.unzip
                        |> Tuple.mapSecond (List.any identity)
            in
            ( ([ Pretty.string "{"
               , if alwaysBreak then
                    doubleLines prettyAttributes
                        |> Pretty.indent 2

                 else
                    Pretty.lines prettyAttributes
                        |> Pretty.indent 2
               , Pretty.string "}"
               ]
                |> Pretty.lines
              )
                |> optionalGroup alwaysBreak
            , alwaysBreak || List.length setters > 1
            )


prettyAttribute : Attribute -> ( Doc t, Bool )
prettyAttribute attr =
    case attr of
        Attribute (Node _ fld) (Node _ val) ->
            let
                ( prettyExpr, alwaysBreak ) =
                    prettyExpressionInner topContext 0 val
            in
            case ( val, alwaysBreak ) of
                ( FunctionExpr _ _, True ) ->
                    ( [ [ prettyAttrPath fld
                        , Pretty.string "="
                        ]
                            |> Pretty.words
                      , prettyExpr
                            |> Pretty.a (Pretty.string ";")
                            |> Pretty.indent 2
                      ]
                        |> Pretty.lines
                        |> optionalGroup alwaysBreak
                    , alwaysBreak
                    )

                _ ->
                    ( [ prettyAttrPath fld
                      , Pretty.string "="
                      , prettyExpr
                      ]
                        |> Pretty.words
                        |> Pretty.a (Pretty.string ";")
                        |> optionalGroup alwaysBreak
                    , alwaysBreak
                    )

        AttributeInheritVariables _ ->
            Debug.todo "prettySetter > AttributeInheritVariables _"

        AttributeInheritFromAttrSet _ _ ->
            Debug.todo "prettySetter > AttributeInheritFromAttrSet _ _"


prettyList : Int -> List (Node Expression) -> ( Doc t, Bool )
prettyList indent exprs =
    case exprs of
        [] ->
            ( Pretty.string "[]", False )

        _ ->
            let
                open : Doc t
                open =
                    Pretty.a Pretty.space (Pretty.string "[")

                close : Doc t
                close =
                    Pretty.a (Pretty.string "]") Pretty.line

                ( prettyExpressions, alwaysBreak ) =
                    List.map (prettyExpressionInner topContext (decrementIndent indent 2)) (denodeAll exprs)
                        |> List.unzip
                        |> Tuple.mapSecond (List.any identity)
            in
            ( prettyExpressions
                |> Pretty.separators ", "
                |> Pretty.surround open close
                |> Pretty.align
                |> optionalGroup alwaysBreak
            , alwaysBreak
            )


prettyRecordAccess : Node Expression -> List (Node Name) -> Maybe (Node Expression) -> ( Doc t, Bool )
prettyRecordAccess (Node _ expr) names maybeDefault =
    let
        ( prettyExpr, alwaysBreak ) =
            prettyExpressionInner bottomContext 4 expr
    in
    ( prettyExpr
        |> Pretty.a dot
        |> Pretty.a (prettyAttrPath names)
        |> Pretty.a
            (case maybeDefault of
                Nothing ->
                    Pretty.empty

                Just (Node _ default) ->
                    [ Pretty.string " or"
                    , prettyExpression default
                    ]
                        |> Pretty.words
            )
    , alwaysBreak
    )



--== Helpers


decrementIndent : Int -> Int -> Int
decrementIndent currentIndent spaces =
    let
        modded : Int
        modded =
            modBy 4 (currentIndent - spaces)
    in
    if modded == 0 then
        4

    else
        modded


dot : Doc t
dot =
    Pretty.string "."


slash : Doc t
slash =
    Pretty.string "/"


quotes : Doc t -> Doc t
quotes doc =
    Pretty.surround (Pretty.char '"') (Pretty.char '"') doc


doubleLines : List (Doc t) -> Doc t
doubleLines =
    Pretty.join (Pretty.a Pretty.line Pretty.line)


escape : String -> String
escape val =
    val
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "${" "\\${"
        |> String.replace "\n" "\\n"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\t" "\\t"


optionalGroup : Bool -> Doc t -> Doc t
optionalGroup flag doc =
    if flag then
        doc

    else
        Pretty.group doc


{-| Calculate a precedence for any operator to be able to know when
parenthesis are needed or not.
When a lower precedence expression appears beneath a higher one, its needs
parenthesis.
When a higher precedence expression appears beneath a lower one, if should
not have parenthesis.
-}
precedence : String -> Int
precedence symbol =
    case symbol of
        "?" ->
            11

        "++" ->
            10

        "*" ->
            9

        "/" ->
            9

        "+" ->
            8

        "-" ->
            8

        "//" ->
            7

        "<" ->
            6

        "<=" ->
            6

        ">" ->
            6

        ">=" ->
            6

        "==" ->
            5

        "!=" ->
            5

        "&&" ->
            4

        "||" ->
            3

        "->" ->
            2

        "|>" ->
            1

        "<|" ->
            1

        _ ->
            0


direction : String -> InfixDirection
direction symbol =
    case symbol of
        "?" ->
            Non

        "++" ->
            Right

        "*" ->
            Left

        "/" ->
            Left

        "+" ->
            Left

        "-" ->
            Left

        "//" ->
            Right

        "<" ->
            Non

        "<=" ->
            Non

        ">" ->
            Non

        ">=" ->
            Non

        "==" ->
            Non

        "!=" ->
            Non

        "&&" ->
            Left

        "||" ->
            Left

        "->" ->
            Right

        "|>" ->
            Left

        "<|" ->
            Right

        _ ->
            Non
