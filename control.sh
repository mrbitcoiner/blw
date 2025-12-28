#!/usr/bin/env bash
####################
set -e
####################
readonly RELDIR="$(dirname ${0})"
readonly HELP_MSG='usage: <build|up|down|build-apk|help>'
readonly CT_NAME='blw'
readonly IMG_NAME='blw'
####################
eprintln()
{
	! [ -z "${1}" ] || eprintln "eprintln: missing message"
	printf "${1}\n" 1>&2
	exit 1
}
build()
{
	podman build \
		-f Containerfile \
		--tag="${IMG_NAME}" \
		${RELDIR}
}
up()
{
	podman run --rm \
		-v=${RELDIR}:/app \
		--name="${CT_NAME}" \
		"localhost/${IMG_NAME}" &
}
down()
{
	podman stop ${CT_NAME} 1>/dev/null 2>&1 || true
}
build_apk()
{
	podman exec ${CT_NAME} bash -c 'cd /app; ./gradlew assembleDebug'
}
####################
case "${1}" in
	build) build ;;
	up) up ;;
	down) down ;;
	build-apk) build_apk ;;
	*) eprintln "${HELP_MSG}" ;;
esac
