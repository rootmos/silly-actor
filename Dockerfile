FROM debian:stretch-slim as builder

RUN apt-get update && apt-get install -y \
    curl ncurses-dev xorg-dev make gcc wget git autoconf libtool

RUN mkdir /chez /build

WORKDIR /chez
ENV CHEZ_VER 9.5

RUN wget https://github.com/cisco/ChezScheme/archive/v${CHEZ_VER}.tar.gz
RUN tar xf v${CHEZ_VER}.tar.gz
WORKDIR /chez/ChezScheme-${CHEZ_VER}

RUN ./configure --installprefix=/chez-dist
RUN make -j2 install

WORKDIR /build
ADD .git .git
ADD .gitmodules .
ADD Makefile .
RUN git submodule init bdwgc
RUN git submodule update bdwgc
RUN make bdwgc

FROM debian:stretch-slim

RUN mkdir -p /silly-actor /silly-actor/bdwgc-dist/include
WORKDIR /silly-actor

COPY --from=builder /chez-dist /chez-dist
COPY --from=builder /build/bdwgc-dist bdwgc-dist
ENV SCHEME_BIN /chez-dist/bin/scheme

RUN apt-get update && apt-get install -y --no-install-recommends \
    git make gcc libncurses5 libc-dev

ADD .git .git
ADD .gitmodules .
RUN git submodule init thunderchez nanopass-framework-scheme
RUN git submodule update thunderchez nanopass-framework-scheme

ADD examples examples
ADD Makefile .
ADD runtime runtime
ADD *.scm ./

ENV INFO 1
ENTRYPOINT ["make"]
