module Main exposing (main)

import Example.Flake
import Html exposing (Html)
import Html.Attributes
import Nix


main : Html msg
main =
    Example.Flake.flake
        |> Nix.toString
        |> Html.text
        |> List.singleton
        |> Html.pre [ Html.Attributes.style "padding" "10px" ]
