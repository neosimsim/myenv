self: super: {
  myXmonad = self.xmonad-with-packages.override {
    packages = self: [ self.xmonad-contrib ];
  };
}
