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
      # (sort-lines nil (string-match "^" (buffer-string) (point)) (string-match "^$" (buffer-string) (point)))
      alchemist
      amx
      avy
      awk-ts-mode
      buffer-move
      company
      counsel
      dhall-mode
      eglot-x
      elixir-ts-mode
      elm-mode
      erlang
      fish-mode
      haskell-mode
      highlight-symbol
      htmlize
      isortify
      ivy
      json-mode
      magit
      mixed-pitch
      monokai-theme
      color-theme-sanityinc-tomorrow
      move-text
      nix-mode
      nix-ts-mode
      nov
      org-cliplink
      osm
      poetry
      pylint
      rust-mode
      rust-playground
      rustic
      spacemacs-theme
      treesit-grammars.with-all-grammars
      typescript-mode
      yasnippet

      # have a look into
      shackle # recommended in https://robert.kra.hn/posts/rust-emacs-setup/#additional-packages
    ];
  };
}
