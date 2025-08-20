FROM haskell:9.4-slim AS builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libsqlite3-dev \
    ca-certificates \
    build-essential \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy cabal files for dependency caching
COPY *.cabal cabal.project ./

# Update cabal and build dependencies
RUN cabal update
RUN cabal configure
RUN cabal build --dependencies-only

# Copy all source code
COPY . .

# Build the application
RUN cabal build
RUN cabal install --install-method=copy --installdir=/app/bin

# Runtime stage
FROM debian:bullseye-slim

# Install runtime dependencies  
RUN apt-get update && apt-get install -y \
    libpq5 \
    libsqlite3-0 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash app

# Copy the executable (adjust name if different)
COPY --from=builder /app/bin/todo-api /usr/local/bin/todo-api

# Create directory for database if needed
RUN mkdir -p /app && chown app:app /app

USER app
WORKDIR /app

# Railway will provide PORT environment variable
EXPOSE $PORT

# Start the application
CMD ["todo-api"]