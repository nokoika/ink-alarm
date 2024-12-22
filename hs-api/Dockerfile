# ビルドステージ
FROM haskell:9.4.8-slim AS builder

ENV APP_HOME=/myapp
ENV PORT=8080

WORKDIR $APP_HOME

COPY stack.yaml stack.yaml.lock package.yaml spla-ical.cabal ./

RUN stack setup --install-ghc
RUN stack build --only-dependencies

COPY . .

# 本番用バイナリのビルド
RUN stack build --copy-bins

# RUN strip /root/.local/bin/spla-ical-exe

# ランタイムステージ
FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y libgmp10 ca-certificates && rm -rf /var/lib/apt/lists/*

ENV APP_HOME=/myapp
ENV PORT=8080

WORKDIR $APP_HOME

# ビルドステージからバイナリをコピー
COPY --from=builder /root/.local/bin/spla-ical-exe .

EXPOSE $PORT

CMD ["./spla-ical-exe"]
