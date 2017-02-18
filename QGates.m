(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["QGates requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`QGates`"];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`QGates`*"];
ClearAll @@ Names["QM`QGates`*"];

(* Define all exposed symbols *)
ProjectionMatrix;
CPhase;
CNot::usage = "\
CNot[numQubits, control, target] is the CNOT gate applied between `control` and `target`, operating over `numQubits` qubits.";

Hadamard;
PauliX;
PauliY;
PauliZ;


Begin["`Private`"];

KP[x_] := x;
KP[x___] := KroneckerProduct @ x;

ProjectionMatrix[numQubits_Integer, y_, x_] := Normal @ SparseArray[
  {{y, x} -> 1},
  {2 ^ numQubits, 2 ^ numQubits}
];

p11 = ProjectionMatrix[1, 1, 1];
p22 = ProjectionMatrix[1, 2, 2];


(* CPhase is the controlled phase gate *)
CPhase[numQubits_Integer, control_Integer, target_Integer] := Block[
  {identities1, identities2},
(* Initialize a list of 2 dimensional identity matrices *)
  identities1 = identities2 = ConstantArray[
    IdentityMatrix @ 2, numQubits
  ];
  (* Replace values where appropriate *)
  identities1[[control]] = p11;
  identities2[[control]] = p22;
  identities2[[target]] = PauliMatrix @ 3;
  (* And finally return the gate: *)
  KroneckerProduct @@ identities1 +
      KroneckerProduct @@ identities2
];
CPhase[numQubits_, {control_, target_}] := CPhase[
  numQubits, control, target
];
CPhase[control_, target_] := CPhase[
  2, control, target
];
CPhase[] := CPhase[2, 1, 2];


(* `CNot` is the controlled-not gate *)
CNot[numQubits_Integer, control_Integer, target_Integer] := Block[
  {identities1, identities2},
(* Initialize a list of 2 dimensional identity matrices *)
  identities1 = identities2 = ConstantArray[
    IdentityMatrix @ 2, numQubits
  ];
  (* Replace values where appropriate *)
  identities1[[control]] = p11;
  identities2[[control]] = p22;
  identities2[[target]] = PauliMatrix @ 1;
  (* And finally return the gate: *)
  KroneckerProduct @@ identities1 +
      KroneckerProduct @@ identities2
];
CNot[numQubits_, {control_, target_}] := CNot[
  numQubits, control, target
];
CNot[control_, target_] := CNot[
  2, control, target
];
CNot[] := CNot[2, 1, 2];


OneQubitGate[numQubits_Integer, target_Integer, matrix_] := Block[
  {identities},
  identities = ConstantArray[
    IdentityMatrix @ 2, numQubits
  ];
  identities[[target]] = matrix;

  (* Return result *)
  KP @@ identities
];

Hadamard[numQubits_Integer, target_Integer] := OneQubitGate[numQubits, target, HadamardMatrix[2]];
Hadamard[numQubits_, {target_}] := Hadamard[numQubits, target];
Hadamard[target_] := Hadamard[1, 1];
Hadamard[] := Hadamard[1, 1];


PauliX[numQubits_Integer, target_Integer] := OneQubitGate[numQubits, target, PauliMatrix[1]];
PauliX[numQubits_, {target_}] := PauliX[numQubits, target];
PauliX[target_] := PauliX[1, 1];
PauliX[] = PauliX[1, 1];

PauliY[numQubits_Integer, target_Integer] := OneQubitGate[numQubits, target, PauliMatrix[2]];
PauliY[numQubits_, {target_}] := PauliY[numQubits, target];
PauliY[target_] := PauliY[1, 1];
PauliY[] = PauliY[1, 1];

PauliZ[numQubits_Integer, target_Integer] := OneQubitGate[numQubits, target, PauliMatrix[3]];
PauliZ[numQubits_, {target_}] := PauliZ[numQubits, target];
PauliZ[target_] := PauliZ[1, 1];
PauliZ[] = PauliZ[1, 1];


(* Protect all package symbols *)
With[{syms = Names["QM`QGates`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];


End[];
EndPackage[];