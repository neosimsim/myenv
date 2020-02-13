#!/bin/sh
# zap pat: kill all processes mathing pat

PATH=/bin:/usr/bin:$HOME/bin
IFS='
'

case $* in
"")	echo 'Usage: zap pattern' 1>&2; exit 1 ;;
-*)	SIG=$1; shift
esac

_pickps() {
	pick `ps -ag | awk 'NR>1' | grep "$*"`
}

kill $SIG `_pickps "$*" | awk '{print $1}'`
