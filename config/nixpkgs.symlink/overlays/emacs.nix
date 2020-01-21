self: super: {
  myEmacs = super.emacsWithPackages (pkgs: with pkgs.melpaStablePackages; [
    evil
  ]);
}
