from util import read, get_openqasm_gates
from rpo.passmanager import level_3_with_contant_pure
from qiskit import QuantumCircuit
from qiskit.transpiler import PassManagerConfig

if __name__ == "__main__":
    seed = 0
    pmconfig = PassManagerConfig(
        initial_layout = None,
        basis_gates = get_openqasm_gates(),
        coupling_map = None,
        backend_properties = None,
        seed_transpiler = seed)

    circ = QuantumCircuit.from_qasm_str(read())
    circ = level_3_with_contant_pure(pmconfig).run(circ)
    print(circ.qasm())
