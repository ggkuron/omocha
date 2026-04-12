FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# System dependencies (GLFW + Xvfb for headless)
RUN apt-get update && apt-get install -y \
    curl \
    git \
    clang \
    libglfw3-dev \
    xvfb \
    && rm -rf /var/lib/apt/lists/*

# Install MoonBit
RUN curl -fsSL https://cli.moonbitlang.com/install/unix.sh | bash
ENV PATH="/root/.moon/bin:$PATH"

# Copy glfw-mbt source
WORKDIR /glfw-mbt
COPY . .

# Update registry and check library compiles
RUN moon update
RUN moon check --target native

# Build and run smoke test with virtual display
WORKDIR /glfw-mbt/examples/smoke_test
RUN moon build src --target native
RUN xvfb-run -a ./_build/native/debug/build/smoke_test.exe
