FROM debian:stable
MAINTAINER Neil Mayhew <neil@mayhew.name>

RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get update \
 && apt-get dist-upgrade -y

# Support stack installing a sandboxed ghc
RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get install -y --no-install-recommends \
      ca-certificates build-essential

# Support building Haskell programs
RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get install -y --no-install-recommends \
      libgmp-dev \
      libtinfo-dev

# Support running hexdump-lazy and its tests
RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get install -y --no-install-recommends \
      bsdmainutils
