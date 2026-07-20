# Erling — Boardet's IRC bot

Erling bridges the [boardet](https://github.com/lgandersen/boardet) web board
and an IRC channel:

- **Board → IRC:** it polls the board's Postgres database and announces every
  new post and splash on the channel.
- **IRC → board:** channel members can submit a board post with a chat command.

It is an Erlang/OTP application: two `gen_server`s (`irc`, `pgsql`) under a
`one_for_one` supervisor. Neither does blocking work in `init/1`, so the bot
starts even when the IRC server or database is momentarily unreachable and
reconnects with exponential backoff.

## IRC commands

The trigger word (default `Erling:`) is configurable. In a channel:

```
Erling: hello                 -> "Hej med dig!"
Erling: post Author##Content  -> stores a board post, replies "As you wish.."
```

In a private message the trigger is omitted:

```
hello
post Author##Content
```

`post` splits on the **first** `##`: the left side is the author, the right
side the content (which may itself contain `##`). Both sides are whitespace-
trimmed and must be non-empty, otherwise Erling replies that it could not parse
the message. Content is stored verbatim — `+` and `%` included (the legacy PHP
url-decoding that used to corrupt those is gone).

## Configuration

Every setting is read from an environment variable at startup, falling back to
the `env` defaults in `src/erling.app.src` when the variable is unset or empty.
Nothing is hardcoded at a call site (see `src/erling_config.erl`).

| Variable             | Default              | Meaning                                   |
|----------------------|----------------------|-------------------------------------------|
| `ERLING_IRC_HOST`    | `chat.freenode.net`  | IRC server host                           |
| `ERLING_IRC_PORT`    | `6697`               | IRC server port                           |
| `ERLING_IRC_SSL`     | `true`               | TLS to the IRC server (`true`/`false`)    |
| `ERLING_IRC_PASS`    | `none`               | server password, or `none` to send no PASS |
| `ERLING_CHANNELS`    | `#erlol`             | comma-separated channel list              |
| `ERLING_NICK`        | `Erling`             | bot nick                                  |
| `ERLING_REALNAME`    | `Erling Hansen`      | bot realname                              |
| `ERLING_TRIGGER`     | `Erling:`            | channel command trigger word              |
| `ERLING_BOARD_URL`   | `https://some.where/`| base URL prefixed to splash announcements |
| `ERLING_DB_HOST`     | `localhost`          | Postgres host                             |
| `ERLING_DB_PORT`     | `5432`               | Postgres port                             |
| `ERLING_DB_USER`     | `username`           | Postgres user                             |
| `ERLING_DB_PASS`     | `password`           | Postgres password                         |
| `ERLING_DB_NAME`     | `databasename`       | Postgres database                         |

When `ERLING_IRC_SSL=true`, the server certificate **is** verified
(`verify_peer` with hostname checking — never `verify_none`). Set
`ERLING_IRC_CACERTFILE` to a PEM bundle to trust a private CA (e.g. a
self-signed IRC server); if it is unset, the system trust store is used.

## Build & run

Requires Erlang/OTP 27+ (developed and tested on OTP 29) and `rebar3`.

```sh
rebar3 compile                 # compile (warnings are errors)
rebar3 eunit                   # run the parser/command unit tests
rebar3 dialyzer                # type analysis
rebar3 as prod release         # assemble a self-contained release under _build/prod/rel/erling
_build/prod/rel/erling/bin/erling foreground   # run it attached, logging to stdout
```

### Container

```sh
podman build -t erling .
podman run --rm \
  -e ERLING_IRC_HOST=irc.example \
  -e ERLING_CHANNELS='#brobye' \
  -e ERLING_DB_HOST=postgres -e ERLING_DB_NAME=boardet \
  -e ERLING_DB_USER=boardet  -e ERLING_DB_PASS=... \
  erling
```

The image is multi-stage: an `erlang:29-alpine` builder assembles the release,
which runs on a matching `alpine:3.23` runtime as a non-root user. No ports are
published — Erling only makes outbound connections. In the Brouwer deployment
it is built and run by the `heyting` Ansible role alongside the other stack
services, pointed at the in-stack `ergo` IRC server and boardet's Postgres.
