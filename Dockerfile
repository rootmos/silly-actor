FROM debian:stretch-slim as chez

RUN mkdir /chez
WORKDIR /chez

RUN apt-get update && apt-get install -y \
    curl ncurses-dev xorg-dev make gcc wget

ENV CHEZ_VER 9.5

RUN wget https://github.com/cisco/ChezScheme/archive/v${CHEZ_VER}.tar.gz
RUN tar xf v${CHEZ_VER}.tar.gz
WORKDIR /chez/ChezScheme-${CHEZ_VER}

RUN ./configure --installprefix=/chez-dist
RUN make -j2 install

FROM debian:stretch-slim

COPY --from=chez /chez-dist /chez-dist
ENV SCHEME_BIN /chez-dist/bin/scheme

RUN mkdir /silly-actor
WORKDIR /silly-actor

RUN apt-get update && apt-get install -y --no-install-recommends \
    git make gcc libncurses5 libc-dev

ADD .git .git
ADD .gitmodules .
RUN git submodule init
RUN git submodule update

ADD examples examples
ADD Makefile .
ADD runtime runtime
ADD tests.scm .
ADD silly-actor.scm .
ADD utils.scm .
ADD c-backend.scm .
ADD runtime.scm .

ENTRYPOINT ["make", "INFO=1"]
