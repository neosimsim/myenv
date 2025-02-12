{ pkgs, config, lib, inputs, ... }: {
  options = {
    myenv.emacs = {
      enable = lib.mkEnableOption ''
        Enables emacs together my configurations.
      '';

      extraConfig = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = ''
          Extra emacs config
        '';
      };
    };
  };

  config = lib.mkIf config.myenv.emacs.enable {
    nixpkgs.overlays = with inputs; [
      emacs-overlay.overlay
    ];

    home.sessionVariables = {
      EDITOR = lib.mkDefault "emacsclient -ca  ''";
    };

    home.file.".config/emacs/snippets" = {
      recursive = true;
      source = ./snippets;
    };

    programs.emacs = {
      enable = true;
      package = lib.mkDefault pkgs.emacs-git-nox;

      overrides = ethis: eprev: {
        eglot-x = ethis.callPackage ./eglot-x.nix { inherit (inputs) eglot-x; };

        combobulate = ethis.trivialBuild {
          pname = "combobulate";
          version = "0.0";
          src = inputs.combobulate;
        };
      };

      extraConfig = lib.strings.concatStringsSep "\n" [
        (builtins.readFile ./init.el)
        ";;; Extra Config"
        config.myenv.emacs.extraConfig
      ];

      extraPackages = epkgs: with epkgs; [
        # (sort-lines nil (string-match "^" (buffer-string) (point)) (string-match "^$" (buffer-string) (point)))
        alchemist
        avy
        awk-ts-mode
        buffer-move
        combobulate
        dhall-mode
        eglot
        eglot-x
        elixir-ts-mode
        fish-mode
        fzf
        geiser-racket
        highlight-symbol
        magit
        mixed-pitch
        modus-themes
        move-text
        nix-ts-mode
        org-cliplink
        osm
        rg
        rust-mode
        rustic
        slime
        treesit-grammars.with-all-grammars
        typescript-mode
        yasnippet
        yasnippet-snippets

        # have a look into
        shackle # recommended in https://robert.kra.hn/posts/rust-emacs-setup/#additional-packages
      ];
    };
  };
}
