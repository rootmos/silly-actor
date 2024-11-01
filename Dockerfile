FROM alpine:3.20 AS chezscheme

WORKDIR /workdir

ADD --checksum=sha256:f5827682fa259c47975ffe078785fb561e4a5c54f764331ef66c32132843685d \
        https://github.com/cisco/ChezScheme/releases/download/v9.6.4/csv9.6.4.tar.gz \
        csv.tar.gz

RUN apk update && apk add \
        libarchive-tools patch \
        build-base ncurses-dev libx11-dev util-linux-dev

RUN bsdtar -xf csv.tar.gz --strip-components=1

RUN ./configure --temproot=/pkg
RUN make install


FROM alpine:3.20 AS bdwgc

RUN apk update && apk add \
        build-base

WORKDIR /workdir

COPY bdwgc .

RUN apk add autoconf
RUN apk add automake
RUN apk add libtool

RUN apk add bash
RUN apk add pkgconf
RUN ./autogen.sh
RUN ./configure --enable-static --disable-threads
RUN make install DESTDIR=/pkg

FROM alpine:3.20

COPY --from=chezscheme /pkg /
COPY --from=bdwgc /pkg /

RUN apk update && apk add \
        make gcc musl-dev \
        ncurses util-linux

WORKDIR /silly-actor

COPY nanopass-framework-scheme nanopass-framework-scheme
COPY thunderchez thunderchez
COPY examples examples
COPY runtime runtime
COPY Makefile *.scm .

RUN make runtime

#ENV TRACE=1
ENV INFO=1

RUN make tests
