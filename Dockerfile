FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

# Prevent asking for user input
ENV DEBIAN_FRONTEND noninteractive

# Update apt
RUN sudo apt-get update
# Install yq
RUN sudo apt-get install -y wget
RUN sudo wget https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -O /usr/bin/yq
RUN sudo chmod +x /usr/bin/yq
# Install pyenv
RUN sudo apt-get install -y make
RUN sudo apt-get install -y build-essential
RUN sudo apt-get install -y libssl-dev
RUN sudo apt-get install -y zlib1g-dev
RUN sudo apt-get install -y libbz2-dev
RUN sudo apt-get install -y libreadline-dev
RUN sudo apt-get install -y libsqlite3-dev
RUN sudo apt-get install -y curl
RUN sudo apt-get install -y llvm 
RUN sudo apt-get install -y libncurses5-dev
RUN sudo apt-get install -y libncursesw5-dev
RUN sudo apt-get install -y xz-utils tk-dev
RUN sudo apt-get install -y libffi-dev
RUN sudo apt-get install -y liblzma-dev
RUN sudo curl https://pyenv.run | bash
# Install R
RUN sudo apt-get install -y r-base
RUN sudo apt-get install -y r-recommended

# user should be already opam, just to be sure
USER opam
WORKDIR /home/opam

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
RUN opam install . --deps-only
# Setup Python environment
ENV HOME  /home/opam
ENV PYENV_ROOT $HOME/.pyenv
ENV PATH $PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH
RUN export PYENV_ROOT="$HOME/.pyenv"
RUN command -v pyenv || export PATH="$PYENV_ROOT/bin:$PATH"
RUN eval "$(pyenv init -)"
RUN eval "$(pyenv virtualenv-init -)"
RUN pyenv install -s 3.7.16
RUN pyenv virtualenv -f 3.7.16 env3.7
RUN eval "$(pyenv init -)"; \
    eval "$(pyenv virtualenv-init -)"; \
    pyenv activate env3.7; \
    pip install --upgrade pip; \
    pip install -r scripts/lib/rpo/requirements.txt
RUN pyenv install -s 3.10.10
RUN pyenv virtualenv -f 3.10.10 env3.10
RUN eval "$(pyenv init -)"; \
    eval "$(pyenv virtualenv-init -)"; \
    pyenv activate env3.10; \
    pip install --upgrade pip; \
    pip install -r requirements.txt
# Install required R packages
RUN sudo Rscript packages.R
# Build the project
RUN eval "$(opam env)"
RUN dune build

COPY README.txt .
CMD cat README.txt; bash