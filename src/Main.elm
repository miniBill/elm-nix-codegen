module Main exposing (main)

import Example.Flake
import Html exposing (Html)
import Nix


main : Html msg
main =
    Example.Flake.flake
        |> Nix.toString
        |> Html.text
