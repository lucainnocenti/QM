(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["ReckDecomposition requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`", {"MaTeX`"}];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`*"];
ClearAll @@ Names["QM`*"];


QState::usage = "\
QState[state] gives the internal iQState representation corresponding to state.";

iQState::usage = "\
iQState[amplitudes, basis] is the internal representation of a quantum state in ket representation.
  *amplitudes*: is the (full) list of amplitudes, each one associated with the correspondend element in *basis*.
  *basis*: list of labels for the basis states. Each label can have any Head, but not be a List of objects, or elements with nested heads (that is, it must have an ArrayDepth equal to 2).
    If the ArrayDepth of *basis* is greater than 2 (equal to 3), the iQState is assumed to represent a state in a tensor product basis, and the *amplitudes* should correspondingly have an ArrayDepth equal to the Length of *basis*.\
";

QPlus;
QDot;
QEnv;

$iQStateAutoNormalize;
$iQStatePrettyPrint;
$iQStatePrettyPrintMagnification;

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

QDensityMatrixToKet::usage = "\
QDensityMatrixToKet[dm] returns the set of probability amplitudes corresponding to the input density matrix, with the first amplitude conventionally assumed to be real.
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
QPartialTranspose::usage = "\
QPartialTranspose[dm, n] computes the partial transpose of the density matrix dm with the respect to the n-th space.
";

QEvolve::usage = "\
QEvolve[qstate, evolutionMatrix] returs the state *qstate* evolved according to the specified *evolutionMatrix*.\
";

QNormalize::usage = "\
blabla
";

PureStateQ::usage = "PureStateQ[state] returns True if state is pure, checking if the trace of the square of state is equal to 1.";

TensorProductToMatrix::usage = "TensorProductToMatrix[asd]";
TensorProductFromMatrix::usage = "TensorProductFromMatrix[matrix, {n1, n2, ...}] does what it should do.";

Begin["`Private`"];

(* fillAmps gives back an Association of label->amplitude rules which spans all of the basis states.
   Example:
     In[1] := fillAmps[ <| 1 -> 1 |>, {1, 2, 3} ]
     Out[1] = <| 1 -> 1, 2 -> 0, 3 -> 0 |>
*)
fillAmps[amps_Association, basis_List] :=
    If[KeyExistsQ[amps, #], amps[#], 0] & /@ basis;


qstateParseAmps[amps_] := Which[
(* return error and abort if no amplitude is provided *)
  amps === None, Message[QState::ampsMissing]; Abort[],
(* return error and abort if something else than an association is given *)
(* Head@amps =!= Association, Message[QState::ampsMustBeAss]; Abort[], *)
(* otherwise, the Association amps is given back, with the labels converted to strings *)
  Head @ amps === Association,
  KeyMap[ToString, amps],
  Head @ amps === List,
  amps,
  True, Message[QState::ampsUnrecognized]; Abort[]
];


qstateParseBasis[basis_] := Which[
  basis === None, None,
  Head@basis =!= List, Message[QState::basisMustBeList]; Abort[],
  True, ToString /@ basis
];


QState::ampsMissing = "The input argument \"Amplitudes\" is mandatory.";
QState::ampsMustBeAss = "The input argument \"Amplitudes\" must be an Association.";
QState::ampsUnrecognized = "Unrecognized format for the amplitudes.";
QState::mismatchAmps = "Some basis labels specified as amplitudes are not included in the list of basis states labels.";

(* QState does not handle direct specification of tensor product bases, for now *)
Options[QState] = {"Amplitudes" -> None, "BasisStates" -> None};
QState[opts : OptionsPattern[]] := With[{
  amps = qstateParseAmps[OptionValue @ "Amplitudes"],
  basis = qstateParseBasis[OptionValue @ "BasisStates"]
},
  Which[
  (* If no basis state is provided, the labels given as "Amplitudes" are used instead *)
    (basis === None) && (Head @ amps === Association),
    iQState[
      Developer`ToPackedArray @ Values @ amps,
      {Keys @ amps}
    ],
    (basis === None) && (Head @ amps === List),
    iQState[
      Developer`ToPackedArray @ amps,
      {ToString /@ Range @ Length @ amps}
    ],
  (* If a list of amplitudes is provided it must have the same length as the list of basis elements specified *)
    (Head @ amps === List) && (Length @ amps != Length @ basis),
    Message[QState::mismatchAmps]; Abort[],
  (* If the length of the amplitudes and basis match we proceed in saving the state into the iQState wrapper *)
    (Head @ amps === List) && (Length @ amps == Length @ basis),
    iQState[
      Developer`ToPackedArray @ amps,
      If[TensorRank[basis] == 1, {basis}, basis]
    ],
  (* The labels used to specify the amplitudes must be included in the ones specified for the basis *)
    ! SubsetQ[basis, Keys @ amps],
    Message[QState::mismatchAmps]; Abort[],
  (* Otherwise, if both amplitudes and basis are provided and are compatible with each other, use the latter to complete the former *)
    True,
    iQState[
      Developer`ToPackedArray[fillAmps[amps, basis]],
      If[
        TensorRank[basis] == 1,
        {basis},
        basis
      ]
    ]
  ]
];
QState[amps_] := QState["Amplitudes" -> amps];
QState[amps_, basis_] := QState["Amplitudes" -> amps, "BasisStates" -> basis];


$iQStatePrettyPrintMagnification = 2;
$iQStatePrettyPrint = True;

ClearAll[qStatePrettyPrint];
qStatePrettyPrint[iQState[amps_List, basis : {__String}]] := MaTeX[
  MapThread[
    Which[
      #1 == 0 // TrueQ, "",
      #1 == 1 // TrueQ, "\\left|" ~~ ToString @ TeXForm @ #2 ~~ "\\right\\rangle",
      True,
      "\\left(" ~~ ToString[TeXForm @ Simplify @ #1] ~~ "\\right)\\left|" ~~ ToString @ TeXForm @ #2 ~~ "\\right\\rangle"
    ] &,
    {amps, basis}
  ] // DeleteCases[""] // Riffle[#, "+"] & // StringJoin,
  Magnification -> $iQStatePrettyPrintMagnification
];
qStatePrettyPrint[iQState[amps_, {basis_List}]] := qStatePrettyPrint[iQState[amps, basis]];
qStatePrettyPrint[iQState[amps_List, bases : {{__String} ..}]] := With[{
  basis = Flatten @ Outer[StringJoin @ Riffle[{##}, ", "] &, Sequence @@ bases]
},
  qStatePrettyPrint[iQState[amps, basis]]
];

$iQStateAutoNormalize = True;
iQState::cannotSumDifferentBases = "Quantum states over different bases cannot be summed together.";
iQState /: Plus[state1_iQState, state2_iQState] := QPlus[state1, state2];
iQState /: Times[x_, iQState[amps_, bases_]] := iQState[x amps, bases];

iQState /: MakeBoxes[iQState[amps_, bases_], StandardForm] := If[TrueQ@$iQStatePrettyPrint,
  ToBoxes @ qStatePrettyPrint@iQState[amps, bases],
  RowBox @ {
    "iQState", "[", ToBoxes@amps, ",", ToBoxes@bases, "]"
  }
];


(* ----------------------- HANDLING OF STATES ALGEBRA ----------------------------- *)
SetAttributes[QEnv, HoldAll];
QEnv[expr_] := With[{
  rules = {Plus -> QPlus}
},
  Unevaluated[expr] /. rules
];
QEnv[expr___] := expr;


SetAttributes[QPlus, Orderless];
QPlus[iQState[amps1_, bases1_], iQState[amps2_, bases2_]] := If[
  bases1 =!= bases2,
  Message[iQState::cannotSumDifferentBases]; Return[$Failed],
  iQState[
    If[TrueQ @ $iQStateAutoNormalize, # / Norm @ #, #]&[
      amps1 + amps2
    ],
    bases1
  ]
];
QPlus[iQState[amps1_, bases1_], -iQState[amps2_, bases2_]] := QPlus[
  iQState[amps1, bases1], iQState[-amps2, bases2]
];
QPlus[iQState[amps1_, bases1_], x_ iQState[amps2_, bases2_]] := QPlus[
  iQState[amps1, bases1], iQState[x amps2, bases2]
];
QPlus[x_ iQState[amps1_, bases1_], y_ iQState[amps2_, bases2_]] := QPlus[
  iQState[x amps1, bases1], iQState[y amps2, bases2]
];

QPlus[iQDensityMatrix[m_, b_], iQDensityMatrix[mm_, bb_]] := If[
  b =!= bb, Message[iQDensityMatrix::cannotSumDifferentBases]; Return[$Failed],
  iQDensityMatrix[
    If[TrueQ @ $iQDensityMatrixAutoNormalize, # / Tr @ #, #]&[
      m + mm
    ],
    b
  ]
];
QPlus[iQDensityMatrix[m_, b_], mm_?MatrixQ] := iQDensityMatrix[m + mm, b];
QPlus[x___] := Plus[x];

QDot::differentBases = "The bases must be equal.";
QDot[iQState[amps1_, bases1_], iQState[amps2_, bases2_]] := If[
  bases1 === bases2,
  Conjugate[amps1] . amps2,
  Message[QDot::differentBases]
];

(* iQStateTP stores the amplitudes in a tensor product structure, i.e. as what you get issuing TensorProduct on the single bases *)
(* If a single argument is provided, nothing happens *)
QTensorProduct[something_] := something;
QTensorProduct[iQStateTP[amps1_, basis1_], iQStateTP[amps2_, basis2_]] := iQStateTP[
  TensorProduct[amps1, amps2],
(* This use of Level is problematic if the basis elements have nested structures, for example if
   basis1 = {{0, 1}, {Cos[x], up}}. The basis elements should therefore be restricted to be simple String objects.*)
  Level[#, {-2}]& @ {basis1, basis2}
];

QTensorProduct[states__iQStateTP] := With[{
  amps = {states}[[All, 1]],
  bases = {states}[[All, 2]]
},
  iQStateTP[
    TensorProduct @@ amps,
    bases
  ]
];

QTensorProduct[iQState[amps1_, basis1_], iQState[amps2_, basis2_]] := iQState[
  Flatten @ KroneckerProduct[amps1, amps2],
(* this Join should always work because if non tensor-product bases should have the form {{whatever}} (with two braces) *)
  Join[basis1, basis2]
];

QTensorProduct[states__iQState] := With[{
  amps = {states}[[All, 1]],
  bases = Join @@ {states}[[All, 2]]
},
  iQState[
    Flatten[KroneckerProduct @@ amps],
    bases
  ]
];


QStateToDensityMatrix[iQStateTP[amps_, basis_]] := With[{len = Length @ basis},
  iQDensityMatrixTP[
    Transpose[
      TensorProduct[amps, Conjugate[amps]],
      Join[Range[1, 2 * len - 1, 2], Range[2, 2 * len, 2]]
    ],
    basis
  ]
];
QStateToDensityMatrix[iQState[amps_, basis_]] := iQDensityMatrix[
  KroneckerProduct[Conjugate[amps], amps],
  basis
];

QDensityMatrixToKet[dmMatrix_?MatrixQ] := Conjugate @ dmMatrix[[1]] / Sqrt @ Abs @ dmMatrix[[1, 1]];
QDensityMatrixToKet[dm_iQDensityMatrix] := iQState[
  QDensityMatrixToKet @ First @ dm,
  Last @ dm
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
TensorProductFromMatrix[matrix_, basis : {{__}..}] := Transpose[
  ArrayReshape[
    matrix,
    Join[#, #] &[Length /@ basis]
  ],
  Join[
    Range[1, 2 * Length @ basis - 1, 2],
    Range[2, 2 * Length @ basis, 2]
  ]
];
TensorProductFromMatrix[matrix_, basisLengths : {__Integer}] := Transpose[
  ArrayReshape[
    matrix,
    Join[#, #] &[basisLengths]
  ],
  Join[
    Range[1, 2 * Length @ basisLengths - 1, 2],
    Range[2, 2 * Length @ basisLengths, 2]
  ]
];


(* QDM2Matrix converts an iQDensityMatrixTP, in which the amplitudes are stored in nested lists output of TensorProduct operations, to a regular matrix. *)
(* This should match when the basis is a tensor product basis, for example if `basis = {{0, 1}, {up, down}}` *)
QDM2Matrix[iQDensityMatrixTP[tp_, basis : {{__}..}]] := TensorProductToMatrix[tp];
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
QDM2Matrix[iQDensityMatrixTP[tp_, basis : {__}]] := tp;


(* QDMFromMatrix produces the nested TensorProduct structure from a regular matrix and the corresponding bases, and embeds as an iQDensityMatrixTP object *)
QDMFromMatrix[matrix_List, basis : {{__}..}] := iQDensityMatrixTP[
  Transpose[
    ArrayReshape[matrix, Join[#, #]&[Length /@ basis]],
    Join[
      Range[1, 2 * Length @ basis - 1, 2],
      Range[2, 2 * Length @ basis, 2]
    ]
  ],
  basis
];
QDMFromMatrix[matrix_List, basis : {__}] := iQDensityMatrixTP[matrix, basis];


(* ===================== UPVALUES FOR iQDensityMatrixTP ======================= *)
iQDensityMatrixTP::wrongDims = "The structure of *tp* is not compatible with that of *matrix*.";
iQDensityMatrixTP /: MatrixForm[dm_iQDensityMatrixTP] := MatrixForm[
  QDM2Matrix @ dm
];

iQDensityMatrixTP /: Dot[iQDensityMatrixTP[tp_, basis_], matrix_?MatrixQ] := If[
(* If the product of the dimensions of the bases of the density matrix does not match the dimensions of *matrix* the product cannot be done. *)
  Times @@ Length /@ basis != Length @ matrix,
  Message[iQDensityMatrixTP::wrongDims],
(* otherwise, carry on with the computation *)
  QDMFromMatrix[TensorProductToMatrix[tp] . matrix, basis]
];
iQDensityMatrixTP /: Dot[matrix_?MatrixQ, iQDensityMatrixTP[tp_, basis_]] := If[
(* If the product of the dimensions of the bases of the density matrix does not match the dimensions of *matrix* the product cannot be done. *)
  Times @@ Length /@ basis != Length @ matrix,
  Message[iQDensityMatrixTP::wrongDims],
(* otherwise, carry on with the computation *)
  QDMFromMatrix[matrix . TensorProductToMatrix[tp], basis]
];
(*iQDensityMatrix /: Dot[matricesBefore__?MatrixQ, iQDensityMatrix[tp_, basis_], matricesAfter__?MatrixQ] := QDMFromMatrix[
  Dot @@ {matricesBefore} . TensorProductToMatrix[tp] . Dot @@ {matricesAfter},
  basis
];*)

iQDensityMatrixTP /: Tr[iQDensityMatrixTP[tp_, _]] := QTr[tp];


(* ===================== UPVALUES FOR iQDensityMatrix ========================= *)
iQDensityMatrix::cannotSumDifferentBases = "Quantum states over different bases cannot be summed together.";
iQDensityMatrix /: MatrixForm[iQDensityMatrix[dm_, bases_]] := MatrixForm[
  dm,
  TableHeadings -> {#, #}& @ If[Length @ bases == 1,
    bases[[1]],
    Flatten @ Outer[
      StringJoin @@ Riffle[
        ToString /@ {##},
        ","
      ]&,
      Sequence @@ bases
    ]
  ]
];
iQDensityMatrix /: Tr[iQDensityMatrix[dm_, _]] := Tr[dm];
iQDensityMatrix /: Dot[matrix_?MatrixQ, iQDensityMatrix[dm_, basis_]] := iQDensityMatrix[
  matrix . dm,
  basis
];
iQDensityMatrix /: Dot[iQDensityMatrix[dm_, basis_], matrix_?MatrixQ] := iQDensityMatrix[
  dm . matrix,
  basis
];
iQDensityMatrix /: Dot[iQDensityMatrix[dm1_, basis1_], iQDensityMatrix[dm2_, basis2_]] := Which[
  basis1 =!= basis2,
  Message[iQDensityMatrix::basesMismatch]; Abort[],
  True,
  iQDensityMatrix[
    dm1 . dm2,
    basis1
  ]
];
$iQDensityMatrixAutoNormalize = True;
iQDensityMatrix /: Plus[dm1_iQDensityMatrix, dm2_iQDensityMatrix] := QPlus[dm1, dm2];
iQDensityMatrix /: Plus[iQDensityMatrix[matrix_, bases_], m_?MatrixQ] := iQDensityMatrix[m + matrix, bases];
iQDensityMatrix /: Times[x_, iQDensityMatrix[matrix_, bases_]] := iQDensityMatrix[x matrix, bases];
iQDensityMatrix /: Eigenvalues[iQDensityMatrix[m_, _]] := Eigenvalues[m];
iQDensityMatrix /: Eigenvectors[iQDensityMatrix[m_, _]] := Eigenvectors[m];


QPartialTrace::wrongDims =
    "The tensor product structure is not compatible with the specified \
index over which to do the partial trace.";
QPartialTrace[k_Integer][state_] := QPartialTrace[state, k];
QPartialTrace[iQDensityMatrix[dm_, basis_], k_Integer] /; Length @ basis == 1 := Tr[dm];
QPartialTrace[iQDensityMatrix[dm_, basis_], k_Integer] := With[{
  qdmTP = QPartialTrace[QDMFromMatrix[dm, basis], k]
},
  iQDensityMatrix[
    TensorProductToMatrix[First @ qdmTP],
    qdmTP[[2]]
  ]
];

QPartialTrace[iQDensityMatrixTP[tp_, basis : {{__}..}], k_Integer] /; Length @ basis == 1 := QTr[tp];
QPartialTrace[iQDensityMatrixTP[tp_, basis : {{__}..}], k_Integer] := Which[
  ! (1 <= k <= Length[basis]), Message[QPartialTrace::wrongDims],
  True,
  With[{
    numDims = Length@Dimensions@tp
  },
    iQDensityMatrixTP[
      Transpose[tp,
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

QPartialTrace[matrix_, basisLengths_, k_Integer] := First@QPartialTrace[
  iQDensityMatrix[
    matrix, Range[0, # - 1] & /@ basisLengths
  ],
  k
];


QPartialTranspose::invalidDim = "The index of the basis for the partial transpose is not valid.";
QPartialTranspose[n_Integer][state_] := QPartialTranspose[state, n];
QPartialTranspose[n_Integer][m_, lengths_] := QPartialTranspose[m, lengths, n];
QPartialTranspose[matrix_?MatrixQ, basisLengths : {__Integer}, n_Integer] := ArrayReshape[#, Dimensions@matrix] &@Transpose[
  ArrayReshape[matrix, Join[#, #] &@basisLengths],
  ReplacePart[
    Range[2 Length@basisLengths],
    {
      n -> n + Length@basisLengths,
      n + Length@basisLengths -> n
    }
  ]
];

QPartialTranspose[iQDensityMatrix[matrix_List, bases_List], dim_Integer] := Which[
  !(1 <= dim <= Length@bases), Message[QPartialTranspose::invalidDim],
  Length@bases == 1, iQDensityMatrix[Transpose@matrix, bases],
  True,
  iQDensityMatrix[QPartialTranspose[matrix, Length /@ bases, dim], bases]
];


QEvolve::dimMismatch = "The input matrix and the basis of the QState must have the same dimension.";
QEvolve[iQState[amps_, basis_], matrix_?MatrixQ] /; Length @ matrix == Length @ amps := iQState[
  matrix . amps,
  basis
];

QEvolve[iQDensityMatrix[matrix_, basis_], u_?MatrixQ] := iQDensityMatrix[
  u . matrix . ConjugateTranspose[u],
  basis
];

QEvolve[iQStateTP[amps_, basis_], matrix_?MatrixQ] := iQStateTP[
  ArrayReshape[
    matrix . Flatten[amps],
    Length /@ basis
  ],
  basis
];

QEvolve[iQDensityMatrixTP[tp_, basis_], u_?MatrixQ] := With[{
  tpMatrix = TensorProductToMatrix[tp]
},
  QDMFromMatrix[
    u . tpMatrix . ConjugateTranspose[u],
    basis
  ]
];
QEvolve[iQDensityMatrixTP[tp_, basis_], u_?TensorQ] := If[
  Dimensions @ u != Dimensions @ tp,
  Message[QEvolve::dimMismatch],
  QEvolve[iQDensityMatrixTP[tp, basis], TensorProductToMatrix[u]]
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


(* QTr handles the trace of a density matrix in TensorProduct nested list form *)
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


QNormalize[iQDensityMatrixTP[tp_, basis_]] := iQDensityMatrixTP[
  tp / QTr[tp],
  basis
];
QNormalize[iQDensityMatrix[matrix_, basis_]] := iQDensityMatrix[
  matrix / Tr[matrix],
  basis
];
QNormalize[iQState[amps_, basis_]] := iQState[
  amps / Norm[amps],
  basis
];

PureStateQ[iQDensityMatrix[matrix_, basis_]] := Chop[N @ Tr[matrix . matrix]] == 1;


(* Protect all package symbols *)
With[{syms = Names["QM`QM`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

(* Unprotect changeable Symbols *)
Unprotect[$iQStateAutoNormalize, $iQStatePrettyPrint, $iQStatePrettyPrintMagnification];

End[];
EndPackage[];