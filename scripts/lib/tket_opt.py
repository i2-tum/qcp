from util import read
from pytket.qasm import circuit_from_qasm_str, circuit_to_qasm_str
from pytket.passes import FullPeepholeOptimise

if __name__ == "__main__":
    circ = circuit_from_qasm_str(read())
    FullPeepholeOptimise().apply(circ)
    print(circuit_to_qasm_str(circ))