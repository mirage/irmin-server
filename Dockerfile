# base
FROM ocaml/opam:debian-ocaml-4.12 as base
RUN sudo apt-get update -y
RUN sudo apt-get install -y git m4 libgmp-dev perl libev-dev pkg-config gnuplot-nox libffi-dev
RUN opam remote set-url default https://github.com/ocaml/opam-repository.git
RUN opam update -y && opam install -y conf-libev lwt optint

RUN sudo mkdir /irmin-server
RUN sudo chown opam /irmin-server

USER opam
WORKDIR /irmin-server
COPY . .

RUN opam install . --deps-only
RUN opam exec -- dune build ./bin/server/server.exe

FROM debian

RUN apt-get update -y
RUN apt-get install -y ca-certificates

ENV PORT=9090
ENV HASH=blake2b
ENV CONTENTS=string

EXPOSE $PORT

COPY --from=base /irmin-server/_build/default/bin/server/server.exe ./irmin-server
COPY --from=base /usr/lib/x86_64-linux-gnu/libgmp* /usr/lib/
COPY --from=base /usr/lib/x86_64-linux-gnu/libev* /usr/lib/
VOLUME /data
CMD [ "sh", "-c", "./irmin-server --uri tcp://0.0.0.0:${PORT} --hash ${HASH} --contents ${CONTENTS} --root /data" ]

