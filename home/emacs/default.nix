{ pkgs, config, lib, ... }:
let
  customEmacs = pkgs.emacsWithPackagesFromUsePackage {
    config = pkgs.substituteAll {
      name = "default.el";
      src = ./init.el;

      inherit (pkgs)
        fish;

      inherit (config.home)
        profileDirectory;
    };

    defaultInitFile = true;
    package = config.myenv.emacs.package;
    extraEmacsPackages = config.myenv.emacs.extraPackages;
  };
in
{
  options = {
    myenv.emacs = {
      enable = lib.mkEnableOption ''
        Enables emacs together my configurations.
      '';

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.emacs-git-nox;
        defaultText = lib.literalExpression "pkgs.emacs";
        example = lib.literalExpression "pkgs.emacs25-nox";
        description = "The Emacs package to use.";
      };

      extraPackages = lib.mkOption {
        default = self: [ ];
        type = lib.hm.types.selectorFunction;
        defaultText = "epkgs: []";
        example = lib.literalExpression "epkgs: [ epkgs.emms epkgs.magit ]";
        description = ''
          Extra packages available to Emacs. To get a list of
          available packages run:
          {command}`nix-env -f '<nixpkgs>' -qaP -A emacsPackages`.
        '';
      };
    };
  };

  config = lib.mkIf config.myenv.emacs.enable {
    home.sessionVariables = {
      EDITOR = lib.mkDefault "emacsclient -ca  ''";
    };

    home.file.".config/emacs/snippets" = {
      recursive = true;
      source = ./snippets;
    };

    home.packages = [ customEmacs ];

    myenv.emacs.extraPackages = epkgs: with epkgs; [
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
      modus-themes
      move-text
      nix-ts-mode
      ob-mermaid
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
}
