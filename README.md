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

<<<<<<< HEAD
### Run
=======
This directory contains the standard header for OpenQASM 2.0 files.
However, this is a modified version adapted to the qasm reader used in
this project. QCP treats more than just the U and the CX gate as basis
gate and defines all other gates that it supports directly as opaque
such that the included file still conforms with the OpenQASM 2.0
grammar.
>>>>>>> 63e8ccd (Correct spelling errors)

To run the tool on a single circuit, you can use the following command.
You need to specify a configuration file that contains several settings for the optimization pass.
Take a look at those located in `configs`.
The tool accepts OpenQASM 2.0 input.
```shell
dune exec --no-build --action-stderr-on-success=must-be-empty -- qcprop -c configs/qcprop64.yml < examples/circ.qasm
```

### Develop

<<<<<<< HEAD
The steps above install a bare minimum of required packages to compile the project.
For the development of the tool, it is convenient to install additional packages by performing the following command.
This enables auto-completion for OCaml files.
```shell
make devel
```
=======
In this directory, the main implementation of all components is located.
The module `qasmreader` defines a parser for QASM files based on the
grammar for OpenQASM 2.0. For this, it utilizes an abstract syntax
defined in the module `abs`. It translates known gates, such as the `cp`
or `ccx` gate into our custom data structure representing circuits.

The data structure for circuits is implemented in the module `circuit`.
Circuits are stored as a list of all gates together with some additional
information. This module also provides functions to convert gates to
strings. The module `optimization` provides an interface for
optimization passes to implement.

As described in the paper, QCP relies on several data structures. An
implementation of the used flat lattice can be found in the module
`flatlattice`. The union-table, also described in the paper, is
implemented in the module `union-table`. Our representation of a quantum
state, together with auxiliary functionality is provided by the module
`quantumstate`.

The module `propagation` brings all those data structures together and
implements the QCP. It consists of two main functions, i.e., `map` and
`apply`. The former maps a gate to a potentially optimized gate,
i.e., with fewer controls, whereas the latter function applies the
(optimized) gate on the tracked quantum state.

The module `qasmwriter` takes a circuit and writes it to stdout with the
help of the conversion functions provided by the `circuit` module. The
modules `util` and `logging` provide several helper functions for
processing data and issuing warnings, respectively.


========================================================================

INFORMATION

The content above describes how to use this docker container. You can
open these instructions at any time by executing

    less ~/README.md

========================================================================
>>>>>>> 63e8ccd (Correct spelling errors)
