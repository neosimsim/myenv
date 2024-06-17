{ pkgs, config, inputs, ... }: {
  programs.emacs = {
    enable = config.myenv.enable;
    package = with pkgs;
      if config.myenv.manageWayland
      # use emacs Pure GTK to make use of Wayland scaling
      then emacs-pgtk
      else
        if config.myenv.guiSupport
        then emacs-git
        else emacs-git-nox;

    overrides = ethis: eprev: {
      eglot-x = ethis.callPackage ./eglot-x.nix { inherit (inputs) eglot-x; };
    };

    extraConfig = builtins.readFile ./init.el;
    extraPackages = epkgs: with epkgs; [
      alchemist
      amx
      avy
      awk-ts-mode
      buffer-move
      company
      counsel
      dhall-mode
      eglot-x
      elisp-format
      elixir-ts-mode
      elm-mode
      elpy
      erlang
      fish-mode
      haskell-mode
      highlight-symbol
      htmlize
      isortify
      ivy
      jq-ts-mode
      json-mode
      kaolin-themes
      magit
      mixed-pitch
      move-text
      nix-mode
      nix-ts-mode
      nov
      org-cliplink
      osm
      pylint
      rust-mode
      rust-playground
      rustic
      spacemacs-theme
      treesit-grammars.with-all-grammars
      typescript-mode
      vimgolf
      yasnippet

      # have a look into
      shackle # recommended in https://robert.kra.hn/posts/rust-emacs-setup/#additional-packages
    ];
  };
}
