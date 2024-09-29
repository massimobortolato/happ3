FROM haskell:9.6.6

WORKDIR /opt/happ3


RUN sh -c 'echo "deb https://apt.postgresql.org/pub/repos/apt focal-pgdg main" > /etc/apt/sources.list.d/pgdg.list' && curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc|gpg --dearmor -o /etc/apt/trusted.gpg.d/postgresql.gpg && apt update && apt install -y libpq-dev && cabal update
RUN cabal update

COPY ./happ3.cabal /opt/happ3/happ3.cabal

RUN cabal build --only-dependencies -j4

COPY . /opt/happ3

RUN cabal clean && cabal install

CMD ["happ3"]
