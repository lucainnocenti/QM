(* Abort for old, unsupported versions of Mathematica *)
If[$VersionNumber < 10,
  Print["QCircuitDrawer requires Mathematica 10.0 or later."];
  Abort[]
];

BeginPackage["QM`QCircuitDrawer`"];

(* Unprotect all package symbols *)
Unprotect @@ Names["QM`QCircuitDrawer`*"];
ClearAll @@ Names["QM`QCircuitDrawer`*"];

convertElementToMatrix;

Begin["`Private`"]


namesToMatricesRules = {
  "H" -> QM`QGates`Hadamard,
  "CNOT" -> QM`QGates`CNot,
  "CPHASE" -> QM`QGates`CPhase
};

(* Take an association specifying a single gate, and convert
   it into the corresponding matrix. *)
convertElementToMatrix[numRails_Integer, elem_Association] := (
  elem["Name"] /. namesToMatricesRules
)[
  numRails,
  elem["Rails"]
];


(* Define the topology of the circuit.
   This is meant to completely characterize the graphical and
   logical features of the circuit to be drawn.*)
initializeCircuitTopology[] := Association[
  "NumberOfRails" -> 3,
  "LinesSeparation" -> 1,
  "LinesWidth" -> 8,
  "GatesSquaresWidth" -> 0.5
];

(* ---- Find objects closest to mouse ---- *)

(*
  findClosestPointOnLine: given a 2d point, obtained from
    the MousePosition["Graphics"], returns the closest coordinate
    that sits on a rail.
*)
findClosestPointOnLine[mousePosition_, circuitTopology_] := {

}


(* Protect all package symbols *)
With[{syms = Names["QM`QCircuitDrawer`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];


End[];
EndPackage[];