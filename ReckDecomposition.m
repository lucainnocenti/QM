(* ::Package:: *)

(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["ReckDecomposition requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`ReckDecomposition`"];

ReckParametersFromMatrix::usage = "\
ReckParametersFromMatrix[matrix] returns a list of associations. Each element of this list corresponds to a U(2) matrix acting on a single pair of modes, or a single-mode phase shifter matrix.";
ReckParametersToMatrix::usage = "\
ReckParametersToMatrix[params] converts a list of associations to the corresponding matrix. It converts each association to the corresponding matrix, and then multiplies all the resulting matrices together IN REVERSE ORDER.";
ReckParametersToMatrices::usage = "
ReckParametersToMatrices[params] converts a list of associations to the list of matrices corresponding to the associations in the list.";


Begin["`Private`"];


directSum[m1_?MatrixQ, m2_?MatrixQ] := ArrayFlatten[{
  {m1, 0},
  {0, m2}
}];


ClearAll[u2GeneralMatrix];
u2GeneralMatrix[label_ : ""] := If[label == "",
  With[{
    t = ToExpression[Context[a] <> "t" <> ToString@label],
    r = ToExpression[Context[a] <> "r" <> ToString@label],
    \[Phi] = ToExpression[Context[a] <> "\[Phi]" <> ToString@label]
  },
    {
      {t, E^(I \[Phi]) r},
      {-E^(-I \[Phi])r, t}
    }
  ],
  {
    {t, E^(I \[Phi]) r},
    {-E^(-I \[Phi])r, t}
  }
];
ClearAll[u2Transformation];
u2Transformation[p_, q_, dim_, label_ : ""] := ReplacePart[IdentityMatrix@dim,
  {
    {p, p} -> u2GeneralMatrix[label][[1, 1]],
    {p, q} -> u2GeneralMatrix[label][[1, 2]],
    {q, p} -> u2GeneralMatrix[label][[2, 1]],
    {q, q} -> u2GeneralMatrix[label][[2, 2]]
  }
];


ClearAll[ReckParametersToMatrices];
ReckParametersToMatrices[params : {Association[__]..}, dim_Integer] := Table[
  If[KeyExistsQ[assoc, "modes"],
    (*Print[u2Transformation[Sequence @@ assoc["modes"], dim]/.{r -> 1231231}];*)
    u2Transformation[Sequence @@ assoc["modes"], dim] /. {
      r -> assoc["r"],
      t -> assoc["t"],
      \[Phi] -> assoc["\[Phi]"]
    },
  (* otherwise, if the current association refers to a phase shift in a single mode: *)
    ReplacePart[IdentityMatrix@dim, {#, #}&@assoc["mode"] -> Exp[I assoc["\[Phi]"]]]
  ],
  {assoc, params}
];

ReckParametersToMatrices[params_Association] := With[{
  dim = Last[params]["mode"] + 1
},
  Table[
    If[KeyExistsQ[layer, "modes"],
      u2Transformation[Sequence @@ layer["modes"], dim] /. {
        r -> layer["r"],
        t -> layer["t"],
        \[Phi] -> layer["\[Phi]"]
      },
      ReplacePart[IdentityMatrix@dim, {#, #}&@layer["mode"] -> Exp[I layer["\[Phi]"]]]
    ],
    {layer, params}
  ]
];

ClearAll[ReckParametersToMatrix];
ReckParametersToMatrix[params_, dim_Integer] := Dot @@ Reverse@ReckParametersToMatrices[params, dim];


ClearAll[vectorToExitInSingleMode];
vectorToExitInSingleMode[u_?UnitaryMatrixQ, outputMode_] := ConjugateTranspose[u].outputMode;


ClearAll[reckParametersFromVector];
reckParametersFromVector[c_, numToAddToModes_Integer : 0] := Module[{outputList = {}},
  Do[
    With[{
      r = Abs[c[[Length@c - layer + 1]]] / Times @@ (outputList[[All, "t"]])
    },
      AppendTo[outputList,
        <|"modes" -> {1 + numToAddToModes, Length@c - layer + 1 + numToAddToModes},
          "r" -> r,
          "t" -> Sqrt[1 - r^2],
          "\[Phi]" -> Pi - Arg[c[[Length@c - layer + 1]]]
        |>
      ]
    ],
    {layer, Length@c - 1}
  ];
  AppendTo[outputList,
    <|"mode" -> 1 + numToAddToModes,
      "\[Phi]" -> Arg@First@c
    |>
  ];
  outputList
];

ClearAll[reckParametersFromMatrix1Step];
reckParametersFromMatrix1Step[u_?UnitaryMatrixQ, numToAddToModes_Integer : 0] := reckParametersFromVector[#, numToAddToModes]&@
    vectorToExitInSingleMode[u, SparseArray[1 -> 1, Length@u]];


ClearAll[reckSimplifyMatrix];
reckSimplifyMatrix[u_?UnitaryMatrixQ] := u.
    ReckParametersToMatrix[#, Length@u]&@reckParametersFromMatrix1Step[u] // Chop;

ClearAll[mapToLowerRightSubmatrix];
mapToLowerRightSubmatrix[f_, matrix_, 1] := f[matrix];
mapToLowerRightSubmatrix[f_, matrix_?MatrixQ, index_Integer] := ArrayFlatten@{
  {matrix[[1 ;; index - 1, 1 ;; index - 1]], matrix[[1 ;; index - 1, index ;;]]},
  {matrix[[index ;;, 1 ;; index - 1]], f@matrix[[index ;;, index ;;]]}
};

(*
  reckParametersFromMatrix takes as input a unitary matrix and generates a list of Associations.
  Each generated Association correspond to a U(2) operation or a single-mode phase shift.
  The elements are generated in left-to-right order, meaning that the corresponding matrix is
  obtained multiplying all the single matrices IN REVERSE ORDER (note that this is already done
  by matrixFromReckParameters).
*)
ClearAll[ReckParametersFromMatrix];
ReckParametersFromMatrix[u_?UnitaryMatrixQ] := Fold[
  mapToLowerRightSubmatrix[
    Function[submatrix,
      Sow@reckParametersFromMatrix1Step[submatrix, #2 - 1];
      reckSimplifyMatrix[submatrix]
    ],
    #1, #2
  ]&,
  u, {1, 2, 3, 4}
] // Reap // Last // Last // Reverse // Apply@Join;


End[]; (* End Private context *)
EndPackage[];
