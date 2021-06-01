#!/bin/sh
while [ $# -gt 0 ]; do
    case "$1" in
    -fg | -bg | -fn)
        XFl="$XFl $1 $2"
        shift ;;
    -*)
        Fl="$Fl $1" ;;
    *)
        break ;;
    esac
    shift
done
xterm $XFl -T "pname $*" -e "pname -M $Fl $*" &
