(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["QM requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`", {"MaTeX`"}];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`*"];
ClearAll @@ Names["QM`*"];


QState::usage = "\
QState[amplitudes] generates a state with the specified amplitudes. The state \
is not renormalized by feault.
QState[ampltudes, bases] generates a state with the specified amplitudes in \
the specified basis.";

QDensityMatrix::usage = "\
QDensityMatrix[amplitudes] generates a density matrix with the specified \
amplitudes, with basis labels automatically generated.
QDensityMatrix[amplitudes, bases] generates a density matrix with the specified \
amplitudes and bases.";

iQState::usage = "\
iQState[amplitudes, basis] is the internal representation of a quantum state in ket representation.
  *amplitudes*: is the (full) list of amplitudes, each one associated with the correspondend element in *basis*.
  *basis*: list of labels for the basis states. Each label can have any Head, but not be a List of objects, or elements with nested heads (that is, it must have an ArrayDepth equal to 2).
    If the ArrayDepth of *basis* is greater than 2 (equal to 3), the iQState is assumed to represent a state in a tensor product basis, and the *amplitudes* should correspondingly have an ArrayDepth equal to the Length of *basis*.";

QStateChangeBasis::usage = "\
QStateChangeBasis[qstate, newbasis] changes the basis of qstate.";

QOpenMap;

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

QPartialTrace::usage = "\
QPartialTrace[matrix, baseIndices, k] computes the partial trace with respect \
to the k-th space, assuming that the dimensions of the tensor product space in \
which matrix lives are the one specified in baseIndices.
QPartialTrace[matrix, baseIndices, indicesToKeep] does the same as above, but \
the third input specified the indices to keep, instead of those to trace out.";

QPartialTranspose::usage = "\
QPartialTranspose[dm, n] computes the partial transpose of the density matrix dm with the respect to the n-th space.
";

QBasePermutation;

QEvolve::usage = "\
QEvolve[qstate, evolutionMatrix] returns the state *qstate* evolved according to the specified *evolutionMatrix*.";

QNormalize::usage = "\
QNormalize[qstate] normalizes the state.";

PureStateQ::usage = "PureStateQ[state] returns True if state is pure, checking if the trace of the square of state is equal to 1.";

TensorProductToMatrix::usage = "TensorProductToMatrix[asd]";
TensorProductFromMatrix::usage = "TensorProductFromMatrix[matrix, {n1, n2, ...}] does what it should do.";

RandomUnitary::usage = "RandomUnitary[m] returns an mxm Haar-random unitary matrix.";

QFidelity::usage = "\
QFidelity[state1, state2] gives the fidelity between the two input states.";

(* Notable quantum states*)

Begin["`Private`"];

(* fillAmps gives back an Association of label->amplitude rules which spans all of the basis states.
   Example:
     In[1] := fillAmps[ <| 1 -> 1 |>, {1, 2, 3} ]
     Out[1] = <| 1 -> 1, 2 -> 0, 3 -> 0 |>
*)
fillAmps[amps_Association, basis_List] :=
    If[KeyExistsQ[amps, #], amps[#], 0] & /@ basis;


qstateParseAmps[amps_] := Which[
  (* Issue Message and abort if no amplitude is provided *)
  amps === None, Message[QState::ampsMissing]; Abort[],

  Head @ amps === Association,
  KeyMap[ToString, amps],
  Head @ amps === List,
  amps,
  True, Message[QState::ampsUnrecognized]; Abort[]
];

(* Checks that if `basis` is not None, then it must be a list *)
qstateParseBasis[basis_] := Which[
  basis === None, None,
  Head @ basis =!= List, Message[QState::basisMustBeList]; Abort[],
  (* If `basis` is a list of lists, we assume it to be specifying a tensor
     product base *)
  MatchQ[basis, {{__}..}],
  Map[ToString, basis, {2}],
  (* else, `basis` is assuemed to be a list of labels *)
  True, ToString /@ basis
];


QState::ampsMissing = "The input argument \"Amplitudes\" is mandatory.";
QState::ampsMustBeAss = "The input argument \"Amplitudes\" must be an Association.";
QState::ampsUnrecognized = "Unrecognized format for the amplitudes.";
QState::mismatchAmps = "Some basis labels specified as amplitudes are not included in the list of basis states labels.";

(* QState does not handle direct specification of tensor product bases, for now *)
Options[QState] = {"Amplitudes" -> None, "BasisLabels" -> None};
(* If not argument or a single String argument is provided, then the evaluation
   is passed on to `notableQStates`, which returns a suitable special iQState.
   With not input arguments, the default behaviour of notableQStates is to
   return the list of accepted names for notable quantum states *)
QState[] := notableQStates[];
QState[notableStateName_String] := notableQStates[notableStateName];

QState[OptionsPattern[]] := With[{
  amps = qstateParseAmps[OptionValue @ "Amplitudes"],
  basis = qstateParseBasis[OptionValue @ "BasisLabels"]
},
  Which[
    (* If no basis state is provided and the amplitudes are given as an association,
       then the keys of the association are used as labels *)
    (basis === None) && (Head @ amps === Association),
    iQState[
      Developer`ToPackedArray @ Values @ amps,
      {Keys @ amps}
    ],
    (* For no base labels and no labels given as Amplitudes, use integers as labels *)
    (basis === None) && (Head @ amps === List),
    iQState[
      Developer`ToPackedArray @ amps,
      {ToString /@ Range @ Length @ amps}
    ],
    (* If the length of the amplitudes and basis match we proceed in saving the state into the iQState wrapper *)
    Head @ amps === List, (
      If[
        (* If basis represents a tensor product space.. *)
        MatchQ[basis, {{__}..}],
        (* Check consistency of dimensions *)
        If[Times @@ Length /@ basis != Length @ amps,
          Message[QState::mismatchAmps];
          Abort[]
        ],
        (* Otherwise, if there is no tensor product structure, check consistency
           dimensions in the simpler case *)
        If[Length @ amps != Length @ basis,
          Message[QState::mismatchAmps];
          Abort[]
        ]
      ];
      (* If the above checks were passed, we can move on to create the state *)
      iQState[
        Developer`ToPackedArray @ amps,
        If[TensorRank[basis] == 1, {basis}, basis]
      ]
    ),
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
QState[amps_, basis_] := QState["Amplitudes" -> amps, "BasisLabels" -> basis];


notableQStates[] := "To implement useful message";
notableQStates[str_String] /; (
  StringMatchQ[str, ("0" | "1")..]
) := QTensorProduct @@ Map[
  QState["Amplitudes" -> <|# -> 1|>, "BasisLabels" -> {0, 1}] &,
  Characters @ str
];
notableQStates["BellStates"] := {
  QState@"00" + QState@"11",
  QState@"00" - QState@"11",
  QState@"01" + QState@"10",
  QState@"01" - QState@"10"
};


QDensityMatrix::basiserr = "The given basis is not valid. It must be a list of \
lists. It was instead `1`.";
QDensityMatrix[matrix_, basis_ : None] := Which[
  (* If no basis is provided, it is assumed that the matrix refers to a state
     living in a single high-dimensional Hilbert space (that is, a single
     qudit) *)
  (basis === None && MatrixQ @ matrix),
  iQDensityMatrix[
    Developer`ToPackedArray @ matrix,
    {ToString /@ Range @ Length @ matrix}
  ],
  True,
  (* Check that the provided basis makes sense, and return with error if it
     doesn't *)
  If[!MatchQ[basis, {{__}..}],
    Message[QDensityMatrix::basiserr, basis];
    Return[$Failed];
  ];
  (* If it does, stringify it and return the iQDensityMatrix object *)
  iQDensityMatrix[
    matrix,
    Map[ToString, basis, {2}]
  ]
];


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
iQState /: Plus[states__iQState] := QPlus[states];
iQState /: Times[x_, iQState[amps_, bases_]] := iQState[x amps, bases];

iQState /: MakeBoxes[iQState[amps_List, bases_List], StandardForm] := If[TrueQ@$iQStatePrettyPrint,
  ToBoxes @ qStatePrettyPrint@iQState[amps, bases],
  RowBox @ {
    "iQState", "[", ToBoxes@amps, ",", ToBoxes@bases, "]"
  }
];


(* QStateChangeBasis
 *
 * Change the basis labels of a state. Returns new state with modified labels.
 *)

QStateChangeBasis[iqstate_iQState, newBasis_] := Module[
  {newiqstate = iqstate, inewBasis = newBasis},
  (* Ensure that all elements of the new basis are strings *)
  inewBasis = Map[ToString, inewBasis, {-1}];
  (* Abort if not a proper basis *)
  If[
    MatchQ[inewBasis, {__String}],
    inewBasis = {inewBasis},
    (* Return failed if the basis is not in an appropriate format *)
    Not @ MatchQ[inewBasis, {{__String}..}],
    Message[QStateChangeBasis::badBasis];
    Return[$Failed];
  ];
  (* Finally actually apply the basis labels change *)
  newiqstate[[2]] = Map[ToString, inewBasis, {-1}];
  newiqstate
];

QStateChangeBasis[newBasis_][iqstate_] := QStateChangeBasis[iqstate, newBasis];

(* ------ HANDLING OF STATES ALGEBRA ------ *)

SetAttributes[QEnv, HoldAll];
QEnv[expr_] := With[{
  rules = {Plus -> QPlus}
},
  Unevaluated[expr] /. rules
];
QEnv[expr___] := expr;


SetAttributes[QPlus, Orderless];
QPlus[states__iQState] := With[{statesList = {states}},
  (* We only check bases for the first two matrices. This should probably be fixed. *)
  If[statesList[[1, 2]] =!= statesList[[2, 2]], Message[iQState::cannotSumDifferentBases]];
  iQState[
    If[TrueQ @ $iQStateAutoNormalize, # / Norm @ #, #]&[
      Total @ statesList[[All, 1]]
    ],
    statesList[[1, 2]]
  ]
];
(* QPlus[iQState[amps1_, bases1_], iQState[amps2_, bases2_]] := If[
  bases1 =!= bases2,
  Message[iQState::cannotSumDifferentBases]; Return[$Failed],
  iQState[
    If[TrueQ @ $iQStateAutoNormalize, # / Norm @ #, #]&[
      amps1 + amps2
    ],
    bases1
  ]
]; *)
QPlus[iQState[amps1_, bases1_], -iQState[amps2_, bases2_]] := QPlus[
  iQState[amps1, bases1], iQState[-amps2, bases2]
];
QPlus[iQState[amps1_, bases1_], x_ iQState[amps2_, bases2_]] := QPlus[
  iQState[amps1, bases1], iQState[x amps2, bases2]
];
QPlus[x_ iQState[amps1_, bases1_], y_ iQState[amps2_, bases2_]] := QPlus[
  iQState[x amps1, bases1], iQState[y amps2, bases2]
];

QPlus[dms__iQDensityMatrix] := With[{dmsList = {dms}},
  (* We only check bases for the first two matrices. This should probably be fixed. *)
  If[dmsList[[1, 2]] =!= dmsList[[2, 2]], Message[iQDensityMatrix::cannotSumDifferentBases]];
  (* Let's sum the individual matrices, and only at the end renormalise *)
  iQDensityMatrix[
    If[TrueQ @ $iQDensityMatrixAutoNormalize, # / Tr @ #, #]&[
      Total @ dmsList[[All, 1]]
    ],
    dmsList[[1, 2]]
  ]
];
(* QPlus[iQDensityMatrix[m_, b_], iQDensityMatrix[mm_, bb_]] := If[
  b =!= bb, Message[iQDensityMatrix::cannotSumDifferentBases]; Return[$Failed],
  iQDensityMatrix[
    If[TrueQ @ $iQDensityMatrixAutoNormalize, # / Tr @ #, #]&[
      m + mm
    ],
    b
  ]
]; *)
QPlus[iQDensityMatrix[m_, b_], mm_?MatrixQ] := iQDensityMatrix[m + mm, b];
QPlus[x___] := Plus[x];

QDot::differentBases = "The bases must be equal.";
QDot[iQState[amps1_, bases1_], iQState[amps2_, bases2_]] := If[
  bases1 === bases2,
  Conjugate[amps1] . amps2,
  Message[QDot::differentBases]
];


(* If a single argument is provided, nothing happens *)
QTensorProduct[something_] := something;
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


QStateToDensityMatrix[iQState[amps_, basis_]] := iQDensityMatrix[
  KroneckerProduct[amps, Conjugate @ amps],
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

(* QDMFromMatrix produces the nested TensorProduct structure from a regular matrix
   and the corresponding bases, and embeds as an iQDensityMatrixTP object. The nested
   TensorProduct structure is useful to compute the partial trace. *)
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
iQDensityMatrix /: Plus[dms__iQDensityMatrix] := QPlus[dms];
(* iQDensityMatrix /: Plus[dm1_iQDensityMatrix, dm2_iQDensityMatrix] := QPlus[dm1, dm2]; *)
iQDensityMatrix /: Plus[iQDensityMatrix[matrix_, bases_], m_?MatrixQ] := iQDensityMatrix[m + matrix, bases];
iQDensityMatrix /: Times[x_, iQDensityMatrix[matrix_, bases_]] := iQDensityMatrix[x matrix, bases];
iQDensityMatrix /: Eigenvalues[iQDensityMatrix[m_, _]] := Eigenvalues[m];
iQDensityMatrix /: Eigenvectors[iQDensityMatrix[m_, _]] := Eigenvectors[m];


QPartialTrace::wrongDims = "The tensor product structure is not compatible with\
 the specified index over which to do the partial trace.";
QPartialTrace[k_Integer][state_] := QPartialTrace[state, k];
QPartialTrace[iQDensityMatrix[matrix_, basis_], k_Integer] /;
  (Length @ basis == 1) := Tr[matrix];
QPartialTrace[iQDensityMatrix[matrix_, basis_], k_Integer] :=
  iQDensityMatrix[
    QPartialTrace[matrix, Length /@ basis, k],
    Drop[basis, {k}]
  ];
QPartialTrace[iQDensityMatrix[matrix_, bases_], indices_List] :=
  iQDensityMatrix[
    QPartialTrace[matrix, Length /@ bases, indices],
    bases[[indices]]
  ];
(* If the first argument is a ket state, it is converted to a density matrix
   and QPartialTrace evaluated over the corresponding density matrix. *)
QPartialTrace[ket_iQState, args__] := QPartialTrace[
  QStateToDensityMatrix[ket], args
];
QPartialTrace[matrix_?MatrixQ, lengths_List, indexToTrace_Integer] :=
  QPartialTrace[matrix,
    lengths,
    Complement[Range @ Length @ lengths, {indexToTrace}]
  ];
QPartialTrace[matrix_?MatrixQ, lengths_List, indicesToKeep_List] := With[{
  indicesToTrace = Complement[Range @ Length @ lengths, indicesToKeep]
},
  With[{
    matrixInTPForm = Transpose[
      ArrayReshape[matrix, Join[lengths, lengths]],
      Join @@ Transpose@Partition[Range[2 Length @ lengths], 2]
    ]
  },
    Flatten[
      TensorContract[
        matrixInTPForm,
        {2 # - 1, 2 #} & /@ indicesToTrace
      ],
      Transpose @ Partition[
        Range[2 Length @ lengths - 2 Length @ indicesToTrace],
        2
      ]
    ]
  ]
];


QPartialTranspose::invalidDim = "The index of the basis for the partial \
transpose is not valid.";
QPartialTranspose[n_Integer][state_] := QPartialTranspose[state, n];
QPartialTranspose[n_Integer][m_, lengths_] := QPartialTranspose[m, lengths, n];
QPartialTranspose[
  matrix_?MatrixQ,
  basisLengths : {__Integer},
  n_Integer
] := ArrayReshape[#, Dimensions@matrix] &@Transpose[
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
  Length@bases == 1, iQDensityMatrix[Transpose @ matrix, bases],
  True,
  iQDensityMatrix[QPartialTranspose[matrix, Length /@ bases, dim], bases]
];


QBasePermutation::badargs = "`1` is not a valid set of arguments.";
QBasePermutation[
  matrix_?MatrixQ,
  basisLengths : {__Integer},
  newIndices : {__Integer}
] := Module[{matrixAsTP, transposedTP},
    (* Convert matrix to TensorProduct structure *)
    matrixAsTP = ArrayReshape[matrix, Join[#, #] & @ basisLengths];
    (* Properly transpose the indices *)
    transposedTP = Transpose[matrixAsTP,
      Join[newIndices, newIndices + Length @ basisLengths]
    ];
    (* Convert back into matrix structure *)
    Flatten[transposedTP,
      {#, # + Length @ basisLengths}& @ Range @ Length @ basisLengths
    ]
];
QBasePermutation[
  iQDensityMatrix[matrix_, basis_],
  newIndices : {__Integer}
] := iQDensityMatrix[
  QBasePermutation[
    matrix,
    Length /@ basis,
    newIndices
  ],
  basis
];
QBasePermutation[args___] := Null /;
  Message[QBasePermutation::badargs, {args}];


QEvolve::dimMismatch = "The input matrix and the basis of the QState must have \
the same dimension.";
QEvolve[iQState[amps_, basis_], matrix_?MatrixQ] /; (
    Length @ matrix == Length @ amps
  ) := iQState[Dot[matrix, amps], basis];
QEvolve[iQDensityMatrix[matrix_, basis_], u_?MatrixQ] := iQDensityMatrix[
  u . matrix . ConjugateTranspose[u],
  basis
];
QEvolve[state_iQState, openMapEvolution : QOpenMap[_List]] := QEvolve[
  QStateToDensityMatrix @ state,
  openMapEvolution
];
QEvolve[iQDensityMatrix[matrix_, _], QOpenMap[eMatrices_List]] := Total @ Table[
  Dot[e, matrix, ConjugateTranspose @ e],
  {e, eMatrices}
];


QNormalize[iQDensityMatrix[matrix_, basis_]] := iQDensityMatrix[
  matrix / Tr[matrix],
  basis
];
QNormalize[iQState[amps_, basis_]] := iQState[
  amps / Norm[amps],
  basis
];

(* Returns True if the density matrix represents a pure state *)
PureStateQ[iQDensityMatrix[matrix_, _]] := Equal[
  Chop[N @ Tr @ Dot[matrix, matrix]],
  1
];


(* Generate a random unitary matrix, drawn from the uniform distribution.
   The method is from Maris Ozols, 'How to generate a random unitary matrix'.
*)
RandomUnitary[m_] := Orthogonalize[
  Map[#[[1]] + I #[[2]]&, #, {2}]& @ RandomReal[
    NormalDistribution[0, 1], {m, m, 2}
  ]
];


(* If the inputs are both matrices, it is assumed they are density matrices*)
QFidelity::dimsMismatch = "The two input density matrices must have the same\
dimensions.";
QFidelity[dm1_?MatrixQ, dm2_?MatrixQ] := (
  Check[Dimensions @ dm1 == Dimensions @ dm2, QFidelity::dimsMismatch];
  Chop @ Tr @ Dot[dm1, dm2]
);
QFidelity[dm1_iQDensityMatrix, dm2_iQDensityMatrix] := QFidelity[
  First @ dm1, First @ dm2
];

(* Compute the fidelity between two input quantum states in vector form*)
QFidelity[amps1_?VectorQ, amps2_?VectorQ] := Abs[Dot[
  Conjugate @ amps1, amps2
]]^2;
QFidelity[ket1_iQState, ket2_iQState] := QFidelity[
  First @ ket1, First @ ket2
];

QFidelity[state1_][state2_] := QFidelity[state1, state2];

(* Protect all package symbols *)
With[{syms = Names["QM`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

(* Unprotect changeable Symbols *)
Unprotect[
  $iQStateAutoNormalize,
  $iQStatePrettyPrint,
  $iQStatePrettyPrintMagnification
];

End[];
EndPackage[];
