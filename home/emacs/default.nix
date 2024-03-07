{ pkgs, config, ... }: {
  programs.emacs = {
    enable = true;
    package = with pkgs;
      if config.myenv.manageSway
      # use emacs Pure GTK to make use of Wayland scaling
      then emacs-pgtk
      else
        if config.myenv.enableGuiTools
        then emacs-git
        else emacs-git-nox;

    extraConfig = builtins.readFile ./init.el;
    extraPackages = epkgs: with epkgs; [
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

      haskell-mode
      nix-mode
      json-mode
      fish-mode
      elpy
      pylint
      isortify
      dhall-mode
      elm-mode
      erlang
      elixir-mode
      alchemist
      rust-mode
      rustic
      rust-playground
      elisp-format

      company
      yasnippet

      # have a look into
      shackle # recommended in https://robert.kra.hn/posts/rust-emacs-setup/#additional-packages
    ];
  };
}
