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

then users will be able to specify their **username**, and with the following commands it will be possible to manage rooms:

- `/rooms` lists all available rooms;
- `/create <room name>` will create a room and make the user join it;
- `/join <room name>` will let a user join the specified room;
- `/leave` will make the user leave the current room;
- `/destroy <room name>` will delete the specified room, only if the user created it;
