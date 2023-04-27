OPENQASM 2.0;
include "qelib1.inc";
qreg q[3];
creg c[3];
h q[0];
cx q[0], q[1];
cx q[1], q[2];
x q[0];
ccx q[0], q[1], q[2]; // this gate will be removed by QCP since the controls cannot be satisfied all at once
x q[0];
cx q[1], q[2];
cx q[0], q[1];
h q[0];
measure q -> c;