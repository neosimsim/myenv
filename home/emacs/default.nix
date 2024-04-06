{ pkgs, config, inputs, ... }: {
  programs.emacs = {
    enable = config.myenv.enable;
    package = with pkgs;
      if config.myenv.useWayland
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
      treesit-grammars.with-all-grammars
      spacemacs-theme
      kaolin-themes
      ivy
      counsel
      amx
      highlight-symbol
      magit
      htmlize
      mixed-pitch
      avy
      move-text
      buffer-move
      osm
      vimgolf
      nov

      eglot-x
      haskell-mode
      nix-mode
      nix-ts-mode
      json-mode
      jq-ts-mode
      fish-mode
      elpy
      pylint
      isortify
      dhall-mode
      elm-mode
      erlang
      elixir-ts-mode
      alchemist
      rust-mode
      rustic
      rust-playground
      elisp-format
      awk-ts-mode

      company
      yasnippet

      # have a look into
      shackle # recommended in https://robert.kra.hn/posts/rust-emacs-setup/#additional-packages
    ];
  };
}
