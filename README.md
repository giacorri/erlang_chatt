# Erlang chat project

Run tests and compile with

```bash
cd chatt
rebar3 ct
rebar3 release
```

then launch server with

```bash
sh chatt/_build/default/rel/chatt/bin/chatt foreground
```

and from other terminals connect to the chat server with

```bash
nc localhost 1234
```
