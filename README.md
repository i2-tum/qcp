# Quantum Constant Propagation (QCP)

*Quantum Constant Propagation* (QCP) implements a quantum equivalent to the classic constant propagation.
It propagates the information about the initial state through the quantum circuit and uses the information made available at each gate to remove superfluous controls or even gates.
The quantum state is split by entanglement groups and is only propagated until the entanglement becomes too complex.
This ensures efficient running time of the optimization.

## Build and Run

### Prerequisites

For the project to clone and compile the following packages need to be installed: `git, opam`

Links to install the mentioned packages are listed below with their version used:
- git (2.34.1): [https://git-scm.com/book/en/v2/Getting-Started-Installing-Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- opam (2.1.2): [https://opam.ocaml.org/doc/Install.html](https://opam.ocaml.org/doc/Install.html)

### Clone Repository

This repository contains submodule(s).
Clone it with:

```shell
git clone --recursive git@github.com:i2-tum/qcp.git
```

### Preparation

The building process of the project requires a functioning OCaml switch with several dependencies installed, for that run:
```shell
make ocaml   # this process takes a couple of minutes
```

### Build

After the steps above, the project can be built either with `dune build` directly or with:
```shell
make build   # this should only take few seconds
```

### Run

To run the tool on a single circuit, you can use the following command.
You need to specify a configuration file that contains several settings for the optimization pass.
Take a look at those located in `configs`.
The tool accepts OpenQASM 2.0 input.
```shell
dune exec --no-build --action-stderr-on-success=must-be-empty -- qcprop -c configs/qcprop64.yml < examples/circ.qasm
```

### Develop

The steps above install a bare minimum of required packages to compile the project.
For the development of the tool, it is convenient to install additional packages by performing the following command.
This enables auto-completion for OCaml files.
```shell
make devel
```
