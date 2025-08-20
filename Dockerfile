FROM ubuntu:22.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
ENV PATH=/opt/.ghcup/bin:$PATH

# Install dependencies and GHCup
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    libpq-dev \
    libsqlite3-dev \
    zlib1g-dev \
    libgmp-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup, GHC, and Cabal
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && /opt/.ghcup/bin/ghcup install ghc 9.6.4 \
    && /opt/.ghcup/bin/ghcup install cabal latest \
    && /opt/.ghcup/bin/ghcup set ghc 9.6.4

WORKDIR /app

# Copy cabal files first
COPY *.cabal cabal.project ./

# Update and build dependencies
RUN /opt/.ghcup/bin/cabal update
RUN /opt/.ghcup/bin/cabal build --dependencies-only

# Copy source and build
COPY . .
RUN /opt/.ghcup/bin/cabal build
RUN /opt/.ghcup/bin/cabal install --install-method=copy --installdir=/app/bin

# Runtime stage
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    libpq5 \
    libsqlite3-0 \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m -s /bin/bash app

COPY --from=builder /app/bin/* /usr/local/bin/

RUN mkdir -p /app && chown app:app /app
USER app
WORKDIR /app

EXPOSE $PORT
CMD ["sh", "-c", "chown app:app /app/data && exec todo-api"]
