FROM ubuntu:22.04

# Prevent asking for user input
ENV DEBIAN_FRONTEND noninteractive

# Update apt
RUN apt update
# Install opam
RUN apt install -y opam
# Install yq
RUN apt install -y wget
RUN wget https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -O /usr/bin/yq
RUN chmod +x /usr/bin/yq
# Install pyenv
RUN apt install -y make
RUN apt install -y build-essential
RUN apt install -y libssl-dev
RUN apt install -y zlib1g-dev
RUN apt install -y libbz2-dev
RUN apt install -y libreadline-dev
RUN apt install -y libsqlite3-dev
RUN apt install -y curl
RUN apt install -y llvm 
RUN apt install -y libncurses5-dev
RUN apt install -y libncursesw5-dev
RUN apt install -y xz-utils tk-dev
RUN apt install -y libffi-dev
RUN apt install -y liblzma-dev
RUN curl https://pyenv.run | bash
# Install R
RUN apt install -y r-base
RUN apt install -y r-recommended

# Copy project files
COPY bin .
COPY configs .
COPY include .
COPY lib .
COPY scripts/lib/rpo/passmanager scripts/lib/rpo/
COPY scripts/lib/rpo/purestate scripts/lib/rpo/
COPY scripts/lib/rpo/purestate scripts/lib/rpo/
COPY scripts/lib/rpo/requirements.txt scripts/lib/rpo/
COPY scripts/lib/bench.R scripts/lib/
COPY scripts/lib/pyzx_opt.py scripts/lib/
COPY scripts/lib/qiskit_opt.py scripts/lib/
COPY scripts/lib/rpo_opt.py scripts/lib/
COPY scripts/lib/rpo_opt.sh scripts/lib/
COPY scripts/lib/tket_opt.py scripts/lib/
COPY scripts/lib/util.py scripts/lib/
COPY scripts/bench.sh scripts/
COPY scripts/bench.yaml scripts/
COPY dune .
COPY dune-project .
COPY Makefile .
COPY packages.R .
COPY requirements.txt .

# Setup OCaml environment
RUN make ocaml
# Setup Python environment
RUN make python
# Install required R packages
RUN make R
# Build the project
RUN make build