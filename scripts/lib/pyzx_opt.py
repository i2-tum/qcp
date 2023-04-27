from util import read
from pyzx import Circuit, extract_circuit
from pyzx.simplify import full_reduce
from pyzx.optimize import full_optimize

if __name__ == "__main__":
    circ = Circuit.from_qasm(read())
    graph = circ.to_graph()
    full_reduce(graph)
    graph.normalize()
    circ = extract_circuit(graph)
    try:
        circ = full_optimize(circ)
    except TypeError:
        pass
    print(circ.to_qasm())
