# base
FROM ocaml/opam:debian-ocaml-4.12 as base
RUN sudo apt-get update -y
RUN sudo apt-get install -y git m4 libgmp-dev perl libev-dev pkg-config gnuplot-x11
RUN opam update

RUN opam install conf-libev irmin-unix

RUN git clone https://github.com/zshipko/irmin-server
RUN opam pin add index.dev --dev-repo
RUN opam pin add irmin-server ./irmin-server --locked

FROM debian

ENV PORT=9090
EXPOSE $PORT

COPY --from=base /home/opam/.opam/4.12/bin/irmin-server .
COPY --from=base /usr/lib/x86_64-linux-gnu/libgmp* /usr/lib/
COPY --from=base /usr/lib/x86_64-linux-gnu/libev* /usr/lib/
VOLUME /data
CMD [ "sh", "-c", "/irmin-server --uri tcp://0.0.0.0:${PORT} --root /data" ]

