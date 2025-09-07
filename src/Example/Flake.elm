module Example.Flake exposing (flake)

import Nix exposing (Expression)
import Nix.Arg
import Nix.Let


flake : Expression
flake =
    Nix.attrSet
        [ ( "description", Nix.string "Home Manager and NixOS configurations" )
        , ( "inputs"
          , Nix.attrSet
                [ ( "nixpkgs.url", Nix.string "github:NixOS/nixpkgs/nixos-25.05" )

                -- , ( "nixpkgs-small.url", Nix.string "github:NixOS/nixpkgs/nixos-25.05-small" )
                -- , ( "nixpkgs-unstable.url", Nix.string "nixpkgs/nixos-unstable" )
                , ( "nixos-hardware.url", Nix.string "github:NixOS/nixos-hardware/master" )
                , ( "systems.url", Nix.string "github:nix-systems/default" )
                , ( "secretdemoclub"
                  , Nix.attrSet
                        [ ( "url", Nix.string "github:miniBill/secretdemoclub?dir=server" )
                        , ( "inputs.nixpkgs.follows", Nix.string "nixpkgs" )
                        , ( "inputs.flake-utils.follows", Nix.string "flake-utils" )
                        ]
                  )
                , ( "musnix"
                  , Nix.attrSet
                        [ ( "url", Nix.string "github:musnix/musnix" )
                        , ( "inputs.nixpkgs.follows", Nix.string "nixpkgs" )
                        ]
                  )
                , ( "agenix"
                  , Nix.attrSet
                        [ ( "url", Nix.string "github:ryantm/agenix" )
                        , ( "inputs.nixpkgs.follows", Nix.string "nixpkgs" )
                        , ( "inputs.home-manager.follows", Nix.string "home-manager" )
                        , ( "inputs.systems.follows", Nix.string "systems" )
                        ]
                  )
                , ( "home-manager"
                  , Nix.attrSet
                        [ ( "url", Nix.string "github:nix-community/home-manager/release-25.05" )
                        , ( "inputs.nixpkgs.follows", Nix.string "nixpkgs" )
                        ]
                  )
                , ( "nix-index-database"
                  , Nix.attrSet
                        [ ( "url", Nix.string "github:nix-community/nix-index-database" )
                        , ( "inputs.nixpkgs.follows", Nix.string "nixpkgs" )
                        ]
                  )
                , ( "nix-index-database"
                  , Nix.attrSet
                        [ ( "url", Nix.string "github:numtide/flake-utils" )
                        , ( "inputs.systems.follows", Nix.string "systems" )
                        ]
                  )
                ]
          )
        , ( "outputs"
          , Nix.fn (Nix.Arg.var "inputs") <|
                \inputs ->
                    Nix.Let.letIn
                        (\withConfig ->
                            Nix.attrSet
                                [ ( "homeConfigurations"
                                  , Nix.attrSet
                                        [ ( "minibill@gadiriel"
                                          , withConfig
                                                (Nix.attrSet
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
                                    [ Nix.attrSet [ ( "pkgs", Nix.null ) ] ]
                            )
                        |> Nix.Let.toExpression
          )
        ]
