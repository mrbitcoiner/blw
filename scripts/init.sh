#!/usr/bin/env bash
####################
set -e
####################
readonly RELDIR="$(dirname ${0})"
####################
eprintln()
{
	! [ -z "${1}" ] || eprintln "eprintln: missing message"
	printf "${1}\n" 1>&2
	exit 1
}
build()
{
	cd /app
	./gradlew assembleDebug
	tail -f /dev/null
}
####################
build
