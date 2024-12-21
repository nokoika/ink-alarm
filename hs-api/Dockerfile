FROM haskell:9.4.8-slim

ENV APP_HOME=/myapp
ENV PORT=8080

WORKDIR $APP_HOME

COPY stack.yaml stack.yaml
COPY package.yaml package.yaml
COPY spla-ical.cabal spla-ical.cabal

RUN stack setup --install-ghc
RUN stack build --only-dependencies

COPY . .

RUN stack build

EXPOSE $PORT

CMD ["stack", "exec", "--", "spla-ical-exe"]
