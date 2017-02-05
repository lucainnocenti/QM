(* ::Package:: *)

(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["BlochSphere requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`BlochSphere`", {"MaTeX`", "utilities`", "QM`QStates`"}];

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
QBlochSphere[OptionsPattern[]] := If[TrueQ@OptionValue@"Labels",
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

QBlochSphereForm[iQState[amps_, bases_]] /; (Length @ bases == 1 && Length @ amps == 2) := Graphics3D[{
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
