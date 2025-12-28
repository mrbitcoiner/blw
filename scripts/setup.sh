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
install_deps()
{
	apt update
	apt install -y --no-install-recommends \
	wget unzip libncurses5 ca-certificates
}
install_java8()
{
	cd /data
	wget "https://corretto.aws/downloads/latest/amazon-corretto-8-x64-linux-jdk.tar.gz"
	tar -xf amazon-corretto-8-x64-linux-jdk.tar.gz
	mkdir -p ~/.local/
	mv amazon-corretto-8.*x64 java
	mv java ~/.local/
}
install_android_cmdline_tools()
{
	cd /data
	wget "https://dl.google.com/android/repository/commandlinetools-linux-8512546_latest.zip"
	unzip commandlinetools-linux-8512546_latest.zip
	mv cmdline-tools ~/.local/
	yes | sdkmanager --sdk_root=${HOME}/.local/share/sdk --licenses
	sdkmanager --sdk_root=${HOME}/.local/share/sdk --install "platforms;android-29" "cmake;3.6.4111459" "ndk;16.1.4479499"
}
clean()
{
	rm -rf /data
}
run()
{
	mkdir -p /data
	install_deps
	install_java8
	install_android_cmdline_tools
	clean

	cd /app
	./gradlew assembleDebug
}
####################
run

