# Quantum Constant Propagation (QCP)

*Quantum Constant Propagation* (QCP) implements a quantum equivalent to the classic constant propagation.
It propagates the information about the initial state through the quantum circuit and uses the information made available at each gate to remove superfluous controls or even gates.
The quantum state is split by entanglement groups and is only propagated until the entanglement becomes too complex.
This ensures efficient running time of the optimization.

## Build and Run

### Prerequisites

For the project to compile the following packages need to be installed: `opam`

To run the benchmark script additionally, the following packages are required: `yq, pyenv, pyenv virtualenv`

If you also want to create the plots afterwards, the following command must be available: `Rscript`

Links to install the mentioned packages are listed below with their version used:
- opam (2.1.0): [https://opam.ocaml.org/doc/Install.html](https://opam.ocaml.org/doc/Install.html)
- yq (4.33.2): [https://github.com/mikefarah/yq#install](https://github.com/mikefarah/yq#install)
- pyenv (2.3.16) and pyenv-virtualenv (1.2.1): [https://realpython.com/intro-to-pyenv/#installing-pyenv](https://realpython.com/intro-to-pyenv/#installing-pyenv)
- R (3.4.4): [https://cran.r-project.org/doc/FAQ/R-FAQ.html#Are-there-Unix_002dlike-binaries-for-R_003f](https://cran.r-project.org/doc/FAQ/R-FAQ.html#Are-there-Unix_002dlike-binaries-for-R_003f)

### Preparation

The building process of the project requires a functioning OCaml switch with several dependencies installed, for that run:
```shell
make ocaml   # this process takes a couple of minutes
```

For the benchmarks to run, additionally a functioning Python environment is needed with the reference tools installed, for that run:
```shell
make python  # this process takes a couple of minutes
```

If you want also the plots to be generated after processing the benchmark circuits, run:
```shell
make R       # this process take few minutes
```

*Note:* If this process causes an error due to missing write permissions, open the R interactive shell by typing `R` in the terminal and pressing enter.
The copy&paste the content of the file `packages.R` into the interactive shell and provide a yes answer to every (two) answer.

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

### Benchmark

If you want to reproduce the results shown in the paper just run the following command. 
Before that, make sure that you satisfy the additional requirements mentioned above.
The script will ask you for one or more inputs depending on whether you run the script for the first time or not, e.g. the number of cores you want to use. 
In case you use 42 cores this process will take approximately 3 hours.
```shell
scripts/bench.sh
```

With the default settings contained in the `bench.yaml` after the script finished there will be an additional directory `bench`.
This contains for every test case a directory named after the key of this test case in the configuration file.
Those directories contain the processed circuits.
Furthermore, there will be a log directory with logs written during the execution of the script and a `result.csv` file containing the results of the evaluation.
If R and the required packages are installed, the plots will be created as `.eps` files in the plot directory.
To generate `.pdf` files from them, run the following command in the plot directory.
This requires the command `ps2pdf` to be loaded.
```shell
 find . -name "*.eps" -execdir ps2pdf -dEPSCrop {} +
```

### Develop

The steps above install a bare minimum of required packages to compile the project.
For the development of the tool, it is convenient to install additional packages by performing the following command.
This enables auto-completion for OCaml files.
```shell
make devel
```

## Project Structure