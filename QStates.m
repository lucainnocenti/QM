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
If the basis is a tensor product basis (that is, its ArrayDepth is 2), than *tp* will have an ArrayDepth equal to twice the number of elements of basis, and thus also twice the ArrayDepth of the corresponding iQState.\
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


QStateToDensityMatrix[iQState[amps_, basis_]] := iQDensityMatrix[
  TensorProduct[amps, Conjugate[amps]],
  basis
];

(* This should match when the basis is a tensor product basis, for example if `basis = {{0, 1}, {up, down}}` *)
QDM2Matrix[iQDensityMatrix[tp_, basis : {{__}..}]] := With[{
  basisProducts = Level[#, {-2}]& @
      Outer[List, Sequence @@ (Range @* Length /@ basis)]
},
  Table[
    tp[[Sequence @@ rowIdx, Sequence @@ colIdx]],
    {rowIdx, basisProducts},
    {colIdx, basisProducts}
  ]
];
(* This is the case for simple basis states, like `basis = {-1, 0, 1}` *)
QDM2Matrix[iQDensityMatrix[tp_, basis : {__}]] := tp;


QDMFromMatrix[matrix_List, basis : {{__}..}] := ArrayReshape[matrix, Length /@ basis];
QDMFromMatrix[matrix_List, basis : {__}] := matrix;


iQDensityMatrix /: MatrixForm[dm_iQDensityMatrix] := MatrixForm[
  QDM2Matrix @ dm
];


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
        Insert[Insert[Range@numDims, numDims - 1, k], numDims,
          k + numDims / 2] // Most // Most
      ] // Map[Tr, #, {2 Length@basis - 2}] &,
      Delete[basis, k]
    ]
  ]
];


(* Protect all package symbols *)
With[{syms = Names["QM`QStates`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

End[];
EndPackage[];