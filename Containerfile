# Erling — the boardet <-> IRC bridge bot (Erlang/OTP).
# Built locally by the heyting role (podman build), like boardet and shell.
#
# Multi-stage: a full Erlang/OTP builder assembles a self-contained release
# (include_erts = true, so the runtime image needs no Erlang installed), which
# is copied into a slim Alpine runtime. Builder OTP major (29) matches the
# bundled ERTS and the OTP the app is tested against.

# --- builder ---------------------------------------------------------------
FROM docker.io/library/erlang:29-alpine AS builder

# rebar3 fetches epgsql from hex (a tarball, no git needed), but keep git for
# any transitive git deps a future change might add.
RUN apk add --no-cache git

WORKDIR /build

# Warm the dep cache on the lockfile alone, so app-source edits don't refetch.
COPY rebar.config rebar.lock ./
RUN rebar3 as prod get-deps

COPY src/ src/
COPY config/ config/
RUN rebar3 as prod release

# --- runtime ---------------------------------------------------------------
# Pinned to the SAME Alpine as erlang:29-alpine (3.23). The bundled ERTS crypto
# NIF is compiled against the builder's OpenSSL; a runtime with an older OpenSSL
# fails to load it ("symbol not found"). Keep these two Alpine versions in step.
FROM docker.io/library/alpine:3.23

# Runtime libs the bundled ERTS links against: C++/ncurses for the emulator,
# openssl for crypto/ssl, ca-certificates for TLS trust + libstdc++.
RUN apk add --no-cache libstdc++ ncurses-libs openssl ca-certificates \
    && adduser -D -u 10001 erling

COPY --from=builder --chown=erling:erling /build/_build/prod/rel/erling /opt/erling

USER erling

# No ports are published: Erling is an outbound-only client (it connects to the
# IRC server and Postgres, nothing connects to it).

# `foreground` runs the release as the container's PID-1-ish process: it stays
# attached, logs to stdout, and exits on SIGTERM so the runtime can restart it.
CMD ["/opt/erling/bin/erling", "foreground"]
