(* ::Package:: *)

(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["ReckDecomposition requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`QStates`"];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`QStates`*"];
ClearAll @@ Names["QM`QStates`*"];


QState::usage = "\
QState[state] gives the internal iQState representation corresponding to state.";

iQState::usage = "\
iQState[amplitudes, basis] is the internal representation of a quantum state in ket representation.
  *amplitudes*: is the (full) list of amplitudes, each one associated with the correspondend element in *basis*.
  *basis*: list of labels for the basis states. Each label can have any Head, but not be a List of objects, or elements with nested heads (that is, it must have an ArrayDepth equal to 2).
    If the ArrayDepth of *basis* is greater than 2 (equal to 3), the iQState is assumed to represent a state in a tensor product basis, and the *amplitudes* should correspondingly have an ArrayDepth equal to the Length of *basis*.\
";

QTensorProduct::usage = "\
QTensorProduct[state1, state2, ...] gives an iQState object representing the state obtained through tensor product of state1 and state2.
  *state1*, *state2*, *statei*: are expected to be iQState objects.\
";

iQDensityMatrix::usage = "\
iQDensityMatrix[tp, basis] is the internal representation of the density matrix with matrix *tp* over the basis *basis*.
If the basis is a tensor product basis (that is, its ArrayDepth is 2), than *tp* will have an ArrayDepth equal to twice the number of elements of basis, and thus also twice the ArrayDepth of the corresponding iQState.
IMPORTANT: the tensor product structure of *tp* is in row1-col1-row2-col2-... order, meaning that for example the element tp[[1, 2, 3, 4]] is the element corresponding at the row (1, 3) and column (2, 4).\
";

QStateToDensityMatrix::usage = "\
QStateToDensityMatrix[state] takes an iQState object, and returns the density matrix of the corresponding state as an iQDensityMatrix object.\
";

QDM2Matrix::usage = "\
QDM2Matrix[iQDensityMatrix[tp, basis]] returns the regular matrix representation of the input density matrix.\
";

QDMFromMatrix::usage = "\
QDMFromMatrix[matrix, basis] converts the 2D matrix *matrix* into the tensor product representation of the corresponding state in the basis *basis*.\
";

QPartialTrace::usage = "\
QPartialTrace[dm, k] computes the partial trace with respect to the *k*-th basis of the density matrix *dm*.\
";

QEvolve::usage = "\
QEvolve[qstate, evolutionMatrix] returs the state *qstate* evolved according to the specified *evolutionMatrix*.\
";

QRenormalize::usage = "\
blabla
";
QTr;

TensorProductToMatrix::usage = "TensorProductToMatrix[asd]";

Begin["`Private`"];

(* fillAmps gives back an Association of label->amplitude rules which spans all of the basis states.
   Example:
     In[1] := fillAmps[ <| 1 -> 1 |>, {1, 2, 3} ]
     Out[1] = <| 1 -> 1, 2 -> 0, 3 -> 0 |>
*)
fillAmps[amps_Association, basis_List] :=
    If[KeyExistsQ[amps, #], amps[#], 0] & /@ basis;


QState::missingAmps = "The amplitudes have not been specified.";
QState::mismatchAmps = "\
Some basis labels specified as amplitudes are not included in the \
list of basis states labels.";
QState[state_List] := QState[Association@state];
QState[state_Association] := Which[
(* A list of amplitudes must necessarily be provided *)
  ! KeyExistsQ[state, "Amplitudes"],
  Message[QState::missingAmps],
(* If no basis state is provided, the labels given as "Amplitudes" are used incstead *)
  ! KeyExistsQ[state, "BasisStates"],
  iQState[Values@state["Amplitudes"], Keys@state["Amplitudes"]],
(* The labels used to specify the amplitudes must be included in the ones specified for the basis *)
  ! SubsetQ[state["BasisStates"], Keys@state["Amplitudes"]],
  Message[QState::mismatchAmps],
  True,
  iQState[fillAmps[state["Amplitudes"], state["BasisStates"]],
    state["BasisStates"]]
];


QTensorProduct[iQState[amps1_, basis1_], iQState[amps2_, basis2_]] := iQState[
  TensorProduct[amps1, amps2],
(* This use of Level is problematic if the basis elements have nested structures, for example if
   basis1 = {{0, 1}, {Cos[x], up}}. The basis elements should therefore be restricted to be simple String objects.*)
  Level[#, {-2}]& @ {basis1, basis2}
];
QTensorProduct[states__iQState] := With[{
  amps = {states}[[All, 1]],
  bases = {states}[[All, 2]]
},
  iQState[
    TensorProduct @@ amps,
    bases
  ]
];


QStateToDensityMatrix[iQState[amps_, basis_]] := With[{len = Length @ basis},
  iQDensityMatrix[
    Transpose[
      TensorProduct[amps, Conjugate[amps]],
      Join[Range[1, 2 * len - 1, 2], Range[2, 2 * len, 2]]
    ],
    basis
  ]
];

(* TensorProductToMatrix assumes that the tensor structure is of the form row,column,row,column,... *)
TensorProductToMatrix[tp_] := With[{len = Length @ Dimensions @ tp},
  Flatten[tp,
    {
      Range[1, len - 1, 2],
      Range[2, len, 2]
    }
  ]
];
(* This should match when the basis is a tensor product basis, for example if `basis = {{0, 1}, {up, down}}` *)
QDM2Matrix[iQDensityMatrix[tp_, basis : {{__}..}]] := With[{len = Length @ basis},
  Flatten[
    tp,
    {
      Range[1, 2 * len - 1, 2],
      Range[2, 2 * len, 2]
    }
  ]
];
(*QDM2Matrix[iQDensityMatrix[tp_, basis : {{__}..}]] := With[{
  basisProducts = Level[#, {-2}]& @
      Outer[List, Sequence @@ (Range @* Length /@ basis)]
},
  Table[
    tp[[Sequence @@ rowIdx, Sequence @@ colIdx]],
    {rowIdx, basisProducts},
    {colIdx, basisProducts}
  ]
];*)
(* This is the case for simple basis states, like `basis = {-1, 0, 1}` *)
QDM2Matrix[iQDensityMatrix[tp_, basis : {__}]] := tp;


QDMFromMatrix[matrix_List, basis : {{__}..}] := iQDensityMatrix[
  Transpose[
    ArrayReshape[matrix, Join[#, #]&[Length /@ basis]],
    Join[
      Range[1, 2 * Length @ basis - 1, 2],
      Range[2, 2 * Length @ basis, 2]
    ]
  ],
  basis
];
QDMFromMatrix[matrix_List, basis : {__}] := iQDensityMatrix[matrix, basis];


iQDensityMatrix::wrongDims = "The structure of *tp* is not compatible with that of *matrix*.";
iQDensityMatrix /: MatrixForm[dm_iQDensityMatrix] := MatrixForm[
  QDM2Matrix @ dm
];

iQDensityMatrix /: Dot[iQDensityMatrix[tp_, basis_], matrix_?MatrixQ] := If[
(* If the product of the dimensions of the bases of the density matrix does not match the dimensions of *matrix* the product cannot be done. *)
  Times @@ Length /@ basis != Length @ matrix,
  Message[iQDensityMatrix::wrongDims],
(* otherwise, carry on with the computation *)
  QDMFromMatrix[TensorProductToMatrix[tp] . matrix, basis]
];
iQDensityMatrix /: Dot[matrix_?MatrixQ, iQDensityMatrix[tp_, basis_]] := If[
(* If the product of the dimensions of the bases of the density matrix does not match the dimensions of *matrix* the product cannot be done. *)
  Times @@ Length /@ basis != Length @ matrix,
  Message[iQDensityMatrix::wrongDims],
(* otherwise, carry on with the computation *)
  QDMFromMatrix[matrix . TensorProductToMatrix[tp], basis]
];
(*iQDensityMatrix /: Dot[matricesBefore__?MatrixQ, iQDensityMatrix[tp_, basis_], matricesAfter__?MatrixQ] := QDMFromMatrix[
  Dot @@ {matricesBefore} . TensorProductToMatrix[tp] . Dot @@ {matricesAfter},
  basis
];*)

iQDensityMatrix /: Tr[iQDensityMatrix[tp_, _]] := QTr[tp];


QPartialTrace::wrongDims =
    "The tensor product structure is not compatible with the specified \
index over which to do the partial trace.";
QPartialTrace[iQDensityMatrix[state_, basis : {{__}..}], k_Integer] := Which[
  ! (1 <= k <= Length[basis]), Message[QPartialTrace::wrongDims],
  True,
  With[{
    numDims = Length@Dimensions@state
  },
    iQDensityMatrix[
      Transpose[state,
        Insert[
          Range[numDims],
          Unevaluated[Sequence @@ Range[numDims - 1, numDims]],
          2 * k - 1
        ] // Most // Most
      ] // Map[Tr, #, {numDims - 2}] &,
      Delete[basis, k]
    ]
  ]
];


QEvolve::dimMismatch = "The input matrix and the basis of the QState must have the same dimension.";
QEvolve[iQState[amps_, basis_], matrix_?MatrixQ] := iQState[
  ArrayReshape[
    matrix . Flatten[amps],
    Length /@ basis
  ],
  basis
];
QEvolve[iQDensityMatrix[tp_, basis_], u_?MatrixQ] := With[{
  tpMatrix = TensorProductToMatrix[tp]
},
  QDMFromMatrix[
    u . tpMatrix . ConjugateTranspose[u],
    basis
  ]
];
QEvolve[iQDensityMatrix[tp_, basis_], u_?TensorQ] := If[
  Dimensions @ u != Dimensions @ tp,
  Message[QEvolve::dimMismatch],
  QEvolve[iQDensityMatrix[tp, basis], TensorProductToMatrix[u]]
];
(* The implementation where we directly act on the tensor indices turns out to be much slower than just converting back to matrices
QEvolve[iQDensityMatrix[tp_, basis_], u_?TensorQ] := If[
*)(* If the tensor product structures are compatible, the products can be done without converting back and forth to matrices *)(*
  Dimensions @ u == Dimensions @ tp,
  With[{
    basisDim = Length @ basis
  },
    iQDensityMatrix[
      Transpose[
        TensorContract[
          TensorProduct[
            u, tp, Conjugate[u]
          ],
          Join[
            {2 #, 2 basisDim + 2 # - 1}& /@ Range[basisDim],
            {2 basisDim + 2 #, 4 basisDim + 2 #}& /@ Range[basisDim]
          ]
        ],
        Join[
          Range[1, 2 basisDim - 1, 2],
          Range[2, 2 basisDim, 2]
        ]
      ],
      basis
    ]
  ]
];
*)


QTr[tp_] := With[{
  lenDims = Dimensions[tp][[Range[1, Length @ Dimensions @ tp - 1, 2]]]
},
  Function[indicesVars,
    Sum[
      tp[[Sequence @@ (Sequence @@ {#, #} & /@ indicesVars)]],
      Evaluate[
        Sequence @@ MapIndexed[
          {#1, lenDims[[First @ #2]]}&,
          indicesVars
        ]
      ]
    ]
  ][
    Array[k, Length @ lenDims]
  ]
];


QRenormalize[iQDensityMatrix[tp_, basis_]] := iQDensityMatrix[
  tp / QTr[tp],
  basis
];

PureStateQ[iQDensityMatrix[tp_, basis_]] := a;


(* Protect all package symbols *)
With[{syms = Names["QM`QStates`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

End[];
EndPackage[];