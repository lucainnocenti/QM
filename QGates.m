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

QOneQubitGate::usage = "\
QOneQubitGate[numQubits, target, matrix] returns the matrix representing the \
action of the one qubit gate `matrix` on the target qubit.";
QTwoQubitGate::usage = "\
QTwoQubitGate[numQubits, control, target, matrix] returns the matrix representing the gate `matrix` between the control and the target qubit.";

Toffoli;
Fredkin;

Begin["`Private`"];

KP[x_] := x;
KP[x___] := KroneckerProduct @ x;

ProjectionMatrix[numQubits_Integer, y_, x_] := Normal @ SparseArray[
  {{y, x} -> 1},
  {2 ^ numQubits, 2 ^ numQubits}
];

p11 = ProjectionMatrix[1, 1, 1];
p22 = ProjectionMatrix[1, 2, 2];


QOneQubitGate[numQubits_Integer, target_Integer, matrix_] := Block[
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
QTwoQubitGate[numQubits_Integer,
              control_Integer,
              target_Integer,
              matrix_
  ] := Block[{tp, matrixAsTP, transposedBigTP},
  (* Convert matrix to a TensorProduct structure *)
  matrixAsTP = Transpose[
    ArrayReshape[matrix,
      {2, 2, 2, 2}], {1, 3, 2, 4}
    ];
  (* Make the rest of the identity matrices comprising the total matrix of the
     gate, and then put everything (identity matrices and restructured `matrix`)
     together with TensorProduct *)
  tp = TensorProduct[
    TensorProduct @@ ConstantArray[IdentityMatrix @ 2, numQubits - 2],
    matrixAsTP
  ];
  (* Transpose the nested List produced by TensorProduct to make `matrix`
     operate on the control and target qubits *)
  transposedBigTP = Transpose[tp,
    Sequence @@ {2 # - 1, 2 #} & /@
        {
          Sequence @@ Complement[Range @ numQubits, {control, target}],
          control,
          target
        }
  ];
  (* Rearrange and flatten indices to convert the TensorProduct into a matrix
     compatible with the output of a KroneckerProduct operation *)
  Flatten[transposedBigTP,
    {
      Range[1, 2 numQubits, 2],
      Range[2, 2 numQubits, 2]
    }
  ]
];

(* defineOneQubitGateFunctions is a "macro" to easily create the downvalues
   associated functions defining one qubit gates *)
Attributes[defineOneQubitGateFunctions] = {HoldRest};
defineOneQubitGateFunctions[name_Symbol, matrix_] := (
  name[numQubits_Integer, target_Integer] := QOneQubitGate[
    numQubits, target, matrix
  ];
  name[numQubits_, {target_}] := name[numQubits, target];
  name[target_] := name[1, 1];
  name[] := name[1, 1];
);


defineOneQubitGateFunctions[Hadamard, HadamardMatrix @ 2];
defineOneQubitGateFunctions[PauliX, PauliMatrix @ 1];
defineOneQubitGateFunctions[PauliY, PauliMatrix @ 2];
defineOneQubitGateFunctions[PauliZ, PauliMatrix @ 3];


(* -------- TWO QUBIT GATES -------- *)

(* CPhase is the controlled phase gate *)
CPhase[] := Plus[
  KP[{{1, 0}, {0, 0}}, IdentityMatrix @ 2],
  KP[{{0, 0}, {0, 1}}, PauliZ[]]
];
CPhase[numQubits_Integer, control_Integer, target_Integer] := QTwoQubitGate[
  numQubits, control, target, CPhase[]
];
CPhase[numQubits_, {control_, target_}] := CPhase[
  numQubits, control, target
];
CPhase[control_, target_] := CPhase[
  2, control, target
];


(* `CNOT` is the controlled-not gate *)
CNot[] := Plus[
  KP[{{1, 0}, {0, 0}}, IdentityMatrix @ 2],
  KP[{{0, 0}, {0, 1}}, PauliX[]]
];
CNot[numQubits_Integer, control_Integer, target_Integer] := QTwoQubitGate[
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
