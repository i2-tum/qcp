gates = [
    'ccx', 'cp', 'cry', 'cswap', 'cu', 'cu1', 'cu3', 'cx', 'cz', 
    'ecr', 'h', 'p', 'rccx', 'rx', 'rxx', 'ry', 'ryy', 'rz', 'rzz', 
    's', 'sdg', 'swap', 'sx', 't', 'tdg', 'u', 'u1', 'u2', 'u3', 'x', 'y', 'z'
]

def get_openqasm_gates ():
    return gates

def read():
    lines = []
    while True:
        try:
            line = input()
        except EOFError:
            break
        lines.append(line)
    return "\n".join(lines)