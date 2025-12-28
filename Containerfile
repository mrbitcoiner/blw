FROM debian:bookworm-slim

ENV JAVA_HOME="/root/.local/java"
ENV PATH="/root/.local/cmdline-tools/bin:/usr/bin:/usr/local/bin:/bin"
ENV ANDROID_HOME="/root/.local/share/sdk"
ENV ANDROID_NDK_HOME="/root/.local/share/sdk/ndk/16.1.4479499"

COPY . /app

RUN /app/scripts/setup.sh

ENTRYPOINT ["/app/scripts/init.sh"]
