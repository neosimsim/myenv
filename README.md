# NeoSimSim Env

# Customizations
You can specify workstation specific settings in $HOME/local/profile, e. g.

	export BROWSER=firefox
	export ACME_VARFONT=/mnt/font/FreeSans/12a/font
	export ACME_FIXFONT=/mnt/font/DejaVuSansMono/13a/font

# Troubleshooting
## nix: `warning: setlocale:
If you get the following error, when envoking nix command, e.g. `nix-shell`

	bash: warning: setlocale: LC_ALL: cannot change locale (en_US.UTF-8)

add the following to `$HOME/local/profile`

	export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

You may have to adopt `/usr/lib/locale`.

When on a non glibc Linux, e.g. alpine try to install

	nix-env -i glibc-locales

and add the following to `$HOME/local/profile`

	export LOCALE_ARCHIVE="$(nix-env --installed --no-name --out-path --query glibc-locales)/lib/locale/locale-archive"

Compare with [NixOS/nix#599](https://github.com/NixOS/nix/issues/599).
