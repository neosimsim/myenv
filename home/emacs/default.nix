{ pkgs, config, lib, inputs, ... }: {
  options = {
    myenv.emacs = {
      enable = lib.mkEnableOption ''
        Enables emacs together my configurations.
      '';
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

      extraConfig = builtins.readFile ./init.el;
      extraPackages = epkgs: with epkgs; [
        # (sort-lines nil (string-match "^" (buffer-string) (point)) (string-match "^$" (buffer-string) (point)))
        alchemist
        amx
        avy
        awk-ts-mode
        buffer-move
        color-theme-sanityinc-tomorrow
        combobulate
        company
        counsel
        dhall-mode
        direnv
        eglot
        eglot-x
        elixir-ts-mode
        elm-mode
        erlang
        fish-mode
        fzf
        haskell-mode
        highlight-symbol
        htmlize
        isortify
        ivy
        leuven-theme
        magit
        material-theme
        mixed-pitch
        modus-themes
        monokai-theme
        move-text
        nix-ts-mode
        nov
        org-cliplink
        osm
        poetry
        pylint
        rg
        rust-mode
        rust-playground
        rustic
        solo-jazz-theme
        spacemacs-theme
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
