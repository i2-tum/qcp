import sys, re, warnings
from util import read, get_openqasm_gates
from qiskit import QuantumCircuit, transpile

def usage():
    print("Usage: python qiskit_opt.py -O{0,1,2,3}", file=sys.stderr)
    exit(-1)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        usage()
    p = re.compile("-O(0|1|2|3)")
    m = p.match(sys.argv[1])
    if not m:
        usage()
    try:
        opt_level = int(m.group(1))
    except ValueError as e:
        print(e, file=sys.stderr)
        usage()
    circ = QuantumCircuit.from_qasm_str(read())
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", category=RuntimeWarning)
        circ = transpile(circ, basis_gates=get_openqasm_gates(), optimization_level=opt_level)
    print(circ.qasm())