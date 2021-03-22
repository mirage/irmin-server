# base
FROM ocaml/opam:debian-ocaml-4.12 as base
RUN sudo apt-get update
RUN sudo apt-get install -y m4 libgmp-dev perl libev-dev pkg-config gnuplot-x11
RUN opam update

RUN opam install conf-libev irmin-unix

COPY . .
RUN opam config exec -- opam pin add irmin-server . --locked

FROM debian
EXPOSE 9090
COPY --from=base /home/opam/.opam/4.12/bin/irmin-server .
COPY --from=base /usr/lib/x86_64-linux-gnu/libgmp* /usr/lib/
COPY --from=base /usr/lib/x86_64-linux-gnu/libev* /usr/lib/
VOLUME /data
ENTRYPOINT [ "/irmin-server" ]
CMD [ "--uri", "tcp://0.0.0.0:9090", \
      "--root", "/data" ]

