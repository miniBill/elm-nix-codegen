module Internal.Format exposing (formatValue, sanitize)

{-|

    This is used as a variable or as a record field.

-}


formatValue : String -> String
formatValue str =
    let
        formatted : String
        formatted =
            String.toLower (String.left 1 str) ++ String.dropLeft 1 str
    in
    sanitize formatted


sanitize : String -> String
sanitize str =
    case str of
        "in" ->
            "in_"

        "type" ->
            "type_"

        "case" ->
            "case_"

        "let" ->
            "let_"

        "module" ->
            "module_"

        "exposing" ->
            "exposing_"

        "where" ->
            "where_"

        "main" ->
            "main_"

        "port" ->
            "port_"

        "as" ->
            "as_"

        "if" ->
            "if_"

        "import" ->
            "import_"

        _ ->
            String.replace "." "" str
