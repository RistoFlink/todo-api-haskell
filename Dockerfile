FROM alpine:3.18 AS builder

# Install Haskell and dependencies
RUN apk add --no-cache \
    ghc \
    cabal \
    musl-dev \
    zlib-dev \
    postgresql-dev \
    sqlite-dev \
    gmp-dev \
    libffi-dev

WORKDIR /app

# Copy cabal files
COPY *.cabal cabal.project ./

# Update cabal and build dependencies
RUN cabal update
RUN cabal configure
RUN cabal build --dependencies-only

# Copy source code and build
COPY . .
RUN cabal build
RUN cabal install --install-method=copy --installdir=/app/bin

# Runtime stage
FROM alpine:3.18

# Install runtime dependencies
RUN apk add --no-cache \
    gmp \
    libffi \
    postgresql-libs \
    sqlite

# Create user
RUN adduser -D -s /bin/sh app

# Copy executable
COPY --from=builder /app/bin/todo-api /usr/local/bin/todo-api

# Create app directory
RUN mkdir -p /app && chown app:app /app

USER app
WORKDIR /app

EXPOSE $PORT

CMD ["todo-api"]