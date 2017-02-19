(* ::Package:: *)

(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["BlochSphere requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`BlochSphere`", {"MaTeX`"}];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`BlochSphere`*"];
ClearAll @@ Names["QM`BlochSphere`*"];

QBlochSphere::usage = "QBlochSphere[pos] draws a Bloch sphere centered at position pos.";
QBlochSphereCoordinates::usage = "\
QBlochSphereCoordinates[amps] gives the cartesian coordinates of the Bloch sphere representation of the qubit state with provided amplitudes.
QBlochSphereCoordinates[qstate] gives the cartesian coordinates of the Bloch sphere representation of the given quantum state.
";
QBlochSphereForm;

Begin["`Private`"];

texStyle = Directive[
  GrayLevel[0],
  {
    FontFamily -> "LM Roman 12", FontSize -> 18,
    FontWeight -> Plain, FontSlant -> Plain
  }
];


splineCircle[m_List, r_, angles_List : {0, 2 \[Pi]}] := Module[{seg, \[Phi], start, end, pts, w, k},
  {start, end} = Mod[angles // N, 2 \[Pi]];
  If[end <= start, end += 2 \[Pi]];
  seg = Quotient[end - start // N, \[Pi] / 2];
  \[Phi] = Mod[end - start // N, \[Pi] / 2];
  If[seg == 4, seg = 3;\[Phi] = \[Pi] / 2];
  pts = r RotationMatrix[start].#& /@ Join[
    Take[{{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}}, 2 seg + 1],
    RotationMatrix[seg \[Pi] / 2].#& /@ {{1, Tan[\[Phi] / 2]}, {Cos[\[Phi]], Sin[\[Phi]]}}
  ];
  If[Length[m] == 2,
    pts = m + #& /@ pts,
    pts = m + #& /@ Transpose@Append[Transpose@pts, ConstantArray[0, Length@pts]]
  ];
  w = Join[
    Take[{1, 1 / Sqrt[2], 1, 1 / Sqrt[2], 1, 1 / Sqrt[2], 1}, 2 seg + 1],
    {Cos[\[Phi] / 2], 1}
  ];
  k = Join[{0, 0, 0}, Riffle[#, #]&@Range[seg + 1], {seg + 1}];
  BSplineCurve[pts, SplineDegree -> 2, SplineKnots -> k, SplineWeights -> w]
] /; Length[m] == 2 || Length[m] == 3;


pointsAndConnection[points_] := Sequence @@ {Sequence @@ Point /@ #, Line @ #} & @ points;

surroundingCircles = GeometricTransformation[
  splineCircle[{0, 0, 0}, 1],
  {
    {RotationMatrix[0, {1, 0, 0}], {0, 0, 0}},
    {RotationMatrix[Pi / 2, {1, 0, 0}], {0, 0, 0}},
    {RotationMatrix[Pi / 2, {0, 1, 0}], {0, 0, 0}}
  }
];

texKet[n_, magnification_ : 2, useMatex_ : True] := If[TrueQ@useMatex,
  MaTeX["\\left|" ~~ ToString@n ~~ "\\right\\rangle",
    Magnification -> magnification
  ],
  Text @ Style[
    StringTemplate["\!\(\*TemplateBox[{\"`1`\"},\n\"Ket\"]\)"][
      ToString@n
    ],
    texStyle
  ]
];
texKet[ns : {__}, magnification_ : 2, useMatex_ : True] := If[TrueQ@useMatex,
  MaTeX[
    Table["\\left|" ~~ ToString@n ~~ "\\right\\rangle", {n, ns}],
    Magnification -> magnification
  ],
  texKet[#, magnification, useMatex] & /@ n
];

Options[QBlochSphere] = {"Labels" -> True};
QBlochSphere[OptionsPattern[]] := If[TrueQ @ OptionValue @ "Labels",
  With[{
    kets = texKet@{0, 1, "+", "-", "L", "R"}
  },
    {
      White, Opacity@0.2, Sphere[{0, 0, 0}, 1],
      Opacity@1, Thickness@0.004, PointSize@0.02,
      Red, pointsAndConnection@{{0, 0, 1}, {0, 0, -1}},
      Blue, pointsAndConnection@{{1, 0, 0}, {-1, 0, 0}},
      Darker@Green, pointsAndConnection@{{0, 1, 0}, {0, -1, 0}},
      Black, Point[{0, 0, 0}],
      Text[kets[[1]], {0, 0, 1.2}],
      Text[kets[[2]], {0, 0, -1.2}],
      Text[kets[[3]], {1.2, 0, 0}],
      Text[kets[[4]], {-1.2, 0, 0}],
      Text[kets[[5]], {0, 1.2, 0}],
      Text[kets[[6]], {0, -1.2, 0}],
      Gray, Thin, surroundingCircles
    }
  ],
  {
    White, Opacity@0.2, Sphere[{0, 0, 0}, 1],
    Opacity@1, Thickness@0.004, PointSize@0.02,
    Red, pointsAndConnection@{{0, 0, 1}, {0, 0, -1}},
    Blue, pointsAndConnection@{{1, 0, 0}, {-1, 0, 0}},
    Darker@Green, pointsAndConnection@{{0, 1, 0}, {0, -1, 0}},
    Black, Point[{0, 0, 0}],
    Gray, Thin, surroundingCircles
  }
];

QBlochSphereCoordinates[iQState[amps_, bases_]] := QBlochSphereCoordinates[amps];
QBlochSphereCoordinates[amps_] /; (Length @ amps == 2) := Block[{
  namps = Normalize[amps Exp[-I Arg[First @ amps]]] // Chop
},
  namps = {1, 2 ArcCos[First @ namps], Arg[Last @ namps]};
  Which[
    MatchQ[namps, {1, 0, _}], {0, 0, 1},
    MatchQ[namps, {1, Pi, _}] , {0, 0, -1},
    True,
    FromSphericalCoordinates @ namps
  ]
];

QBlochSphereForm[x_] := (Echo @ MatchQ[x, _iQState]; FullForm @ x)
QBlochSphereForm[
  iQState[amps_, bases_]
] /; (Length @ bases == 1 && Length @ amps == 2) := Graphics3D[{
  QBlochSphere[],
  {
    Point @ #,
    Arrow @ {{0, 0, 0}, #}
  }& @ QBlochSphereCoordinates[amps]
},
  Boxed -> False
];

(* Protect all package symbols *)
With[{syms = Names["QM`BlochSphere`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

End[]; (* End Private context *)
EndPackage[];
