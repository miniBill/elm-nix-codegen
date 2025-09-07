module Example.Flake exposing (flake)

import Nix exposing (Expression)
import Nix.Arg
import Nix.Let


flake : Expression
flake =
    Nix.record
        [ ( "description", Nix.string "Home Manager and NixOS configurations" )
        , ( "inputs"
          , Nix.record
                [ ( "nixpkgs.url", Nix.string "github:NixOS/nixpkgs/nixos-25.05" )

                -- , ( "nixpkgs-small.url", Nix.string "github:NixOS/nixpkgs/nixos-25.05-small" )
                -- , ( "nixpkgs-unstable.url", Nix.string "nixpkgs/nixos-unstable" )
                , ( "nixos-hardware.url", Nix.string "github:NixOS/nixos-hardware/master" )
                , ( "systems.url", Nix.string "github:nix-systems/default" )
                , ( "secretdemoclub"
                  , Nix.record
                        [ ( "url", Nix.string "github:miniBill/secretdemoclub?dir=server" )
                        , ( "inputs.nixpkgs.follows", Nix.string "nixpkgs" )
                        , ( "inputs.flake-utils.follows", Nix.string "flake-utils" )
                        ]
                  )
                ]
          )
        , ( "outputs"
          , Nix.fn (Nix.Arg.var "inputs") <|
                \inputs ->
                    Nix.Let.letIn
                        (\withConfig ->
                            Nix.record
                                [ ( "homeConfigurations"
                                  , Nix.record
                                        [ ( "minibill@gadiriel"
                                          , withConfig
                                                (Nix.record
                                                    [ ( "system", Nix.string "aarch64-darwin" )
                                                    , ( "module", Nix.path "./machines/gadiriel/home-manager.nix" )
                                                    ]
                                                )
                                          )
                                        ]
                                  )
                                ]
                        )
                        |> Nix.Let.fn "withConfig"
                            (Nix.Arg.record (\system username module_ -> { system = system, username = username, module_ = module_ }) { open = False }
                                |> Nix.Arg.field "system"
                                |> Nix.Arg.fieldWithDefault "username" (Nix.string "minibill")
                                |> Nix.Arg.field "module"
                            )
                            (\{ system, username, module_ } ->
                                Nix.apply
                                    (inputs |> Nix.get "home-manager.lib.homeManagerConfiguration")
                                    [ Nix.record [ ( "pkgs", Nix.null ) ] ]
                            )
                        |> Nix.Let.toExpression
          )
        ]
