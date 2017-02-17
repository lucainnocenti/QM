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
InitializeQCircuit;
DrawQCircuit;
QCircuit;

Begin["`Private`"]

Protect[QCircuit];

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


(* Define the topology and elements of the circuit.
   This is meant to completely characterize the graphical and
   logical features of the circuit to be drawn.*)
InitializeQCircuit[] := QCircuit[<|
  "NumberOfQubits" -> 3,
  "LinesSeparation" -> 1,
  "LinesWidth" -> 8,
  "GatesSquaresWidth" -> 0.5,
  "Gates" -> {
    {1.3, <|"Name" -> "H", "Args" -> 1|>},
    {2.7, <|"Name" -> "H", "Args" -> 2|>},
    {4.1, <|"Name" -> "NiceDot", "Args" -> 2|>}
  }
|>];


(* ---- Initialize and draw the rails representing the qubits ---- *)
initializeRails[nOfLines_Integer, linesLength_?NumericQ, linesdy_?NumericQ] := Table[
  {
    {0, linesdy * i},
    {linesLength, linesdy * i}
  },
  {i, nOfLines}
];

drawRails[QCircuit[circuit_]] := With[{
  rails = initializeRails[
    circuit["NumberOfQubits"],
    circuit["LinesWidth"],
    circuit["LinesSeparation"]
  ]
},
  {Thick, Line /@ rails}
];


(* ---- Build all the graphics primitives for the 1 qubit gates ---- *)
draw1QubitGateBox[letter_String] := With[
  {width = 0.2, height = 0.2, useMaTeX = True},
  {
    {
      FaceForm[White], EdgeForm[Directive[Black, Thick]],
      Rectangle[-{width, height} / 2, + {width, height} / 2]
    },
    If[useMaTeX == True,
      Inset[MaTeX`MaTeX[letter], {0, 0}, Automatic, Scaled[0.8]],
      Inset[Style[letter, FontSize -> 200], {0, 0}]
    ]
  }
];
draw1QubitGateBox[pos : {x_, y_}, letter_String, QCircuit[circuit_]] := Inset[
  Graphics @ draw1QubitGateBox[letter],
  pos,
  Automatic,
  circuit["GatesSquaresWidth"]
];

(* drawNiceDot simply draws a simple black dot. It's for debugging purposes. *)
drawNiceDot[pos : {x_, y_}] := Inset[
  Graphics @ {
    PointSize @ .05, Point @ {0, 0}
  },
  pos, Automatic
];


(* ---- Build the graphics primitives for all the gates in the circuit ---- *)
drawGates[QCircuit[circuit_]] := Map[
  Which[
  (* Nice black dot *)
    #[[2]]["Name"] == "NiceDot",
    drawNiceDot[{First @ #, Last[#]["Args"]}],
  (* Hadamard gates *)
    #[[2]]["Name"] == "H",
    draw1QubitGateBox[{#[[1]], #[[2]]["Args"]}, "\\mathcal{H}",
      QCircuit@circuit],
  (* Z Pauli gates *)
    #[[2]]["Name"] == "Z",
    draw1QubitGateBox[{#[[1]], #[[2]]["Args"]}, "Z", QCircuit @ circuit]
  ] &,
  circuit["Gates"]
];


(* Define the options to use in the final graphics *)
graphicsOptions[circuit_QCircuit] := {
  ImageSize -> 500,
  PlotRangePadding -> .2,
  PlotRange -> {
    {0, First[circuit]["LinesWidth"]},
    {
      First[circuit]["LinesSeparation"] - 0.1,
      First[circuit]["LinesSeparation"] * First[circuit]["NumberOfQubits"] + 0.1
    }
  },
  Frame -> True,
  FrameTicks -> None
};


(* ---- Find objects closest to mouse ---- *)

(*
  findClosestPointOnLine: given a 2d point, obtained from
    the MousePosition["Graphics"], returns the closest coordinate
    that sits on a rail.
*)
findClosestPointOnLine[pos : {x_, y_}, QCircuit[circuit_]] := {
  x,
  Round[y / circuit["LinesSeparation"]] * circuit["LinesSeparation"]
};


Attributes[eventHandling] = {HoldAll};
eventHandling[circuit_] /; MatchQ[circuit, _QCircuit] := {
  "MouseDown" :> Which[
  (* Add a nice black point *)
    TrueQ[action == "AddPoint"],
    With[{newPointPos = findClosestPointOnLine[MousePosition["Graphics"], circuit]},
      AppendTo[
        circuit[[1, "Gates"]],
        {
          First @ newPointPos,
          <|"Name" -> "NiceDot", "Args" -> newPointPos[[2]]|>
        }
      ]
    ],
  (* Add a 1 qubit gate *)
    MatchQ[action, {"Add1QubitGate", _String}],
    With[{newPointPos = findClosestPointOnLine[MousePosition["Graphics"], circuit]},
      AppendTo[
        circuit[[1, "Gates"]],
        {
          First @ newPointPos,
          <|"Name" -> action[[2]], "Args" -> newPointPos[[2]]|>
        }
      ]
    ],
    True,
    Print["Nothing to do"]
  ]
};


optionsBar := RadioButtonBar[Dynamic @ action, {
  "AddPoint" -> "Add point",
  {"Add1QubitGate", "H"} -> "Add H",
  {"Add1QubitGate", "Z"} -> "Add Z"
(*"addCPHASE" -> "Add CPHASE",*)
(*"removeCPHASE" -> "Remove CPHASE",*)
(*"addH" -> "Insert H gate",*)
(*"removeH" -> "Remove H gate"*)
},
  Enabled -> True,
  Appearance -> "Vertical"
];


(* Draw the actual circuit.
   Basically all the rest of this file builds up the necessary components for this function. *)
Attributes[DrawQCircuit] = {HoldAll};
DrawQCircuit[circuit_] /; MatchQ[circuit, _QCircuit] := DynamicModule[{
  mousePosition,
  action,
  temporaryStatus
},
  Row[#, "  "] & @ {
    Panel @ optionsBar,
    EventHandler[#, eventHandling @ circuit]&[
      Dynamic @ Graphics[{
        drawRails @ circuit,
        drawGates @ circuit
      },
        Sequence @@ graphicsOptions[circuit]
      ]
    ]
  }
];

(* Protect all package symbols *)
With[{syms = Names["QM`QCircuitDrawer`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];


End[];
EndPackage[];