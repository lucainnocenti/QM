(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["QGates requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`utilities`"];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`utilities`*"];
ClearAll @@ Names["QM`utilities`*"];

(* Define all exposed symbols *)
KP;
convertSymbolListToPatternSequence;
defineParametricFunction;

Begin["`Private`"];

KP[x_] := x;
KP[x___] := KroneckerProduct @ x;

convertSymbolListToPatternSequence[{pars__Symbol}] := Block[{f},
  Apply[Sequence,
    Map[Hold, {pars}] /. Hold[s_] :> (f_ /. f -> s)
  ]
];
convertSymbolListToPatternSequence[pars__Symbol] :=
  convertSymbolListToPatternSequence[{pars}];

Attributes[defineParametricFunction] = HoldAll;
defineParametricFunction[expr_SetDelayed, flag_Symbol, {pars__Symbol}] := ReleaseHold[
  Hold[expr] /. flag -> convertSymbolListToPatternSequence @ pars
];

(* Protect all package symbols *)
With[{syms = Names["QM`utilities`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

End[];
EndPackage[];
