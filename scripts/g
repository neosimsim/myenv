#!/bin/sh

version=8.8.2
list=

usage() {
cat <<EOF
g [-v GHCVERSION] CMD...
g -l
g -h

Runs the given command with the given GHC version set as prefix of PATH.
Expects ghc to be installed in $APPLICATIONS/ghc-$GHCVERSION.

-v GHCVERSION	specify the GHC version (default: $version)
-l		list installed ghc versions
-h		print this help
EOF
exit 1
}

while getopts v:l name
do
	case $name in
	v) version="$OPTARG" ;;
	l) list=1 ;;
	?) usage ;;
	esac
done
shift $(($OPTIND -1))

if [ -n "$list" ]
then
	ls -1 "$APPLICATIONS" | grep -E '^ghc-'
	exit 0
fi

printf 'running `%s` within ghc-%s\n' "$*" "$version"

ghcpath="$APPLICATIONS/ghc-$version"

if ! [ -d "$ghcpath" ]
then
	printf '%s not present' "$ghcpath"
fi

env PATH="$ghcpath/bin:$PATH" "$@"
