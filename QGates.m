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

TwoQubitGate::usage = "\
TwoQubitGate[numQubits, control, target, matrix] returns the matrix representing the gate `matrix` between the control and the target qubit.";
Toffoli;

Begin["`Private`"];

KP[x_] := x;
KP[x___] := KroneckerProduct @ x;

ProjectionMatrix[numQubits_Integer, y_, x_] := Normal @ SparseArray[
  {{y, x} -> 1},
  {2 ^ numQubits, 2 ^ numQubits}
];

p11 = ProjectionMatrix[1, 1, 1];
p22 = ProjectionMatrix[1, 2, 2];


OneQubitGate[numQubits_Integer, target_Integer, matrix_] := Block[
  {identities},
  identities = ConstantArray[
    IdentityMatrix @ 2, numQubits
  ];
  identities[[target]] = matrix;

  (* Return result *)
  KP @@ identities
];


(* To compute a two qubit gate between any pair of qubits,
   we first write the matrix describing the two-qubit gate
   acting on the last two qubits, and then permute the indices
   appropriately.*)
TwoQubitGate[numQubits_Integer, control_Integer, target_Integer, matrix_] := Block[
  {tp, matrixAsTP, transposedBigTP},
(* Initialize a list of 2 dimensional identity matrices *)
  matrixAsTP = Transpose[ArrayReshape[matrix, {2, 2, 2, 2}], {1, 3, 2, 4}];
  tp = TensorProduct[
    TensorProduct @@ ConstantArray[IdentityMatrix @ 2, numQubits - 2],
    matrixAsTP
  ];
  transposedBigTP = Transpose[tp,
    Sequence @@ {2 # - 1, 2 #} & /@
        {
          Sequence @@ Complement[Range @ numQubits, {control, target}],
          control,
          target
        }
  ];
  (* Rearrange indices and flatten TensorProduct into a KroneckerProduct structure *)
  Flatten[transposedBigTP,
    {
      Range[1, 2 numQubits, 2],
      Range[2, 2 numQubits, 2]
    }
  ]
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


(* -------- TWO QUBIT GATES -------- *)


(* CPhase is the controlled phase gate *)
CPhase[] := Plus[
  KP[{{1, 0}, {0, 0}}, IdentityMatrix @ 2],
  KP[{{0, 0}, {0, 1}}, PauliZ[]]
];
CPhase[numQubits_Integer, control_Integer, target_Integer] := TwoQubitGate[
  numQubits, control, target, CPhase[]
];
CPhase[numQubits_, {control_, target_}] := CPhase[
  numQubits, control, target
];
CPhase[control_, target_] := CPhase[
  2, control, target
];


(* `CNot` is the controlled-not gate *)
CNot[] := Plus[
  KP[{{1, 0}, {0, 0}}, IdentityMatrix @ 2],
  KP[{{0, 0}, {0, 1}}, PauliX[]]
];
CNot[numQubits_Integer, control_Integer, target_Integer] := TwoQubitGate[
  numQubits, control, target, CNot[]
];
CNot[numQubits_, {control_, target_}] := CNot[
  numQubits, control, target
];
CNot[control_, target_] := CNot[
  2, control, target
];


(* -------- THREE QUBIT GATES -------- *)


Toffoli[] := SparseArray[
  {
    {i_, i_} /; i <= 6 -> 1,
    {7, 8} -> 1, {8, 7} -> 1
  },
  {8, 8}
];


Fredkin[] := SparseArray[
  {
    {i_, i_} /; i < 8 -> 1,
    {8, 8} -> -1
  },
  {8, 8}
];



(* Protect all package symbols *)
With[{syms = Names["QM`QGates`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];


End[];
EndPackage[];