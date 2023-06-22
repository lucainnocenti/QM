BeginPackage["QM`ReckDecomposition`"];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`ReckDecomposition`*"];
ClearAll @@ Names["QM`ReckDecomposition`*"];


reckDecompositionUnitariesFromUnitary::usage = "reckDecompositionUnitariesFromUnitary[unitary] decomposes a unitary into a sequence of 2x2 unitaries."


Begin["`Private`"]


(* creates a 2x2 unitary whose first column is (the renormalised version of) the input *)
dim2unitaryWithVectorAsFirstColumn[vec_] := 
    With[{normalizedVec = Normalize@vec}, 
        Transpose@{normalizedVec, Conjugate@Cross@normalizedVec}
    ];

find2modeUnitaryToRemoveElement[unitary_, rowInFocus_, colInFocus_] :=
    dim2unitaryWithVectorAsFirstColumn@Cross@{
        unitary[[rowInFocus, colInFocus]],
        unitary[[rowInFocus, rowInFocus]]
    };

embedMatrixInLargerSpace[smallMatrix_, largeSize_, modes_] :=
    ReplacePart[
        IdentityMatrix@largeSize,
        Flatten[#, 1] &@Table[
            Rule[{modes[[i]], modes[[j]]}, smallMatrix[[i, j]]],
            {i, 2}, {j, 2}
        ]
    ];

removeElementFromUnitary[unitary_, {row_, col_}] :=
    With[{m = find2modeUnitaryToRemoveElement[unitary, row, col]}, 
        (* return two outcomes:
        1) first output is the matrix with the element removed;
        2) second output is the 2x2 unitary used to remove it, together with the modes it operates on
        *)
        {
            Dot[unitary,
                embedMatrixInLargerSpace[m, Length@unitary, Sort@{row, col}]
            ],
            {m, Sort@{row, col}}
        }
    ];

   
(* produces a list of target pairs of indices; these are used by `removeElementFromUnitary` to know the order in which elements should be removed from the initial interferometer unitary, in order to obtain a final identity matrix (up to phases) *)
listOfIndicesToRemove[matrixDimension_] := Flatten[#,1]& @ Table[
  {row,col},
  {row, Reverse @ Range[2, matrixDimension]},
  {col, Reverse @ Range[row - 1]}
];


reckDecompositionUnitariesFromUnitary[unitary_] := Fold[
    With[{output = removeElementFromUnitary[#1, #2]},
        Sow[output[[2]]];
        output[[1]]
    ] &,
    unitary,
    listOfIndicesToRemove@Length@unitary
] // Reap // {
    #[[1]],
    #[[2, 1]]
} &;



(* Protect all package symbols *)
With[{syms = Names["QM`ReckDecomposition`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

End[]; (* End Private context *)
EndPackage[];
