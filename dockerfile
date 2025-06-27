# ---------- Build & Test Stage ----------
FROM erlang:25 as builder

WORKDIR /app
COPY chatt /app

# Run tests
RUN rebar3 ct

# Build the release
RUN rebar3 release

# ---------- Runtime Stage ----------
FROM erlang:25-slim

WORKDIR /app
COPY --from=builder /app/_build /app/_build
COPY --from=builder /app/config /app/config

CMD ["/app/_build/default/rel/chatt/bin/chatt", "foreground"]
