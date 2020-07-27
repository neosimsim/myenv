# dotfiles
Collection of my personal dotfiles.

# Install
To install dotfiles call

	make install

This will symlink all \*.symlink files to your $HOME. To select a subset of
\*.symlink call, e.g.

	make DOTFILES=vim install
