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

namesToLabelsRules = {
  "H" -> "\\mathcal{H}",
  "Z" -> "Z"
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
    {1.3, <|"Type" -> "1QubitGate", "Name" -> "H", "Args" -> 1|>},
    {2.7, <|"Type" -> "1QubitGate", "Name" -> "H", "Args" -> 2|>},
    {4.1, <|"Type" -> "Debug", "Name" -> "NiceDot", "Args" -> 2|>}
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


tokenCNOTControl[{x_, y_}] := With[{r = 0.1},
  {
    PointSize @ 0.02,
    Point @ {x, y}
  }
];
tokenCNOTTarget[{x_, y_}] := With[{r = 0.1},
  {
    Thickness @ 0.004,
    Circle[{x, y}, r],
    Line @ {{x - r, y}, {x + r, y}},
    Line @ {{x, y - r}, {x, y + r}}
  }
];

drawCNOTGate[{x_, {controlY_, targetY_}}, QCircuit[circuit_]] := {
  tokenCNOTControl @ {x, controlY},
  tokenCNOTTarget @ {x, targetY},
  Line @ {{x, controlY}, {x, targetY}}
};

drawCPHASEGate[{x_, {controlY_, targetY_}}, QCircuit[circuit_]] := {
  tokenCNOTControl @ {x, controlY},
  tokenCNOTControl @ {x, targetY},
  Line @ {{x, controlY}, {x, targetY}}
};


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
  (* 1 Qubit Gates *)
    #[[2]]["Type"] == "1QubitGate",
    draw1QubitGateBox[
      {#[[1]], #[[2]]["Args"]},
      #[[2]]["Name"] /. namesToLabelsRules,
      QCircuit @ circuit
    ],
  (* CNOT 2 qubit gate *)
    And[
      #[[2]]["Type"] == "2QubitGate",
      #[[2]]["Name"] == "CNOT"
    ],
    drawCNOTGate[
      {#[[1]], #[[2, "Args"]]},
      QCircuit @ circuit
    ],
  (* CPHASE 2 qubit gate *)
    And[
      #[[2]]["Type"] == "2QubitGate",
      #[[2]]["Name"] == "CPHASE"
    ],
    drawCPHASEGate[
      {#[[1]], #[[2, "Args"]]},
      QCircuit @ circuit
    ]
  ] &,
  circuit["Gates"]
];


drawTemporaryStuff[circuit_QCircuit, stuff_Association, action_] := With[{
  mp = MousePosition["Graphics"]
},
  Which[
  (* If the Add1QubitGate radio button is active, and the
       ctrl modifier is pressed, then we are in delete 1 qubit
       gate mode. We highlight the 1 qubit gate closest to the
       mouse accordingly.
    *)
    And[
      MatchQ[action, {"Add1QubitGate", _String}],
      MemberQ[CurrentValue["ModifierKeys"], "Control"]
    ],
    draw1QubitGateBox[
      {#[[1]], #[[2]]["Args"]},
      #[[2]]["Name"] /. namesToLabelsRules,
      circuit
    ] &[
      circuit[[1, "Gates", findGateCloserToMouse[circuit]]]
    ] /. {Black -> Red},
    MatchQ[action, {"Adding2QubitGate", _String}],
    {
      PointSize @ 0.02, Red, Point @ stuff["2QubitGateInitialPoint"],
      Thick,
      Line @ {
        stuff["2QubitGateInitialPoint"],
        {stuff["2QubitGateInitialPoint"][[1]], findClosestPointOnLine[mp, circuit][[2]]}
      }
    }
  ]
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
pointOnLine[qcircuit_QCircuit] := findClosestPointOnLine[
  MousePosition["Graphics"], qcircuit
];


findGateCloserToMouse[circuit_QCircuit] := With[{
  gatesCoordinates = Cases[circuit[[1, "Gates"]],
    {
      x_,
      <|OrderlessPatternSequence["Type" -> "1QubitGate", "Args" -> y_, __]|>
    } :> {x, y}
  ]
},
  First @ Select[
    Range @ Length @ circuit[[1, "Gates"]],
    circuit[[1, "Gates", #, 1]] == Nearest[gatesCoordinates, MousePosition @ "Graphics"][[1, 1]] &
  ]
];

(*
findGateCloserToMouse[circuit_QCircuit] := If[Length@# > 0, First@#, 0] &@With[{
  xDistances = Abs[pos[[1]] - First /@ hgates]
},
  Select[Range@Length@hgates,
    Abs[pos[[1]] - hgates[[#, 1]]] == Min[xDistances] &
  ]
]
*)


Attributes[addSingleRailObjectToQCircuit] = {HoldAll};
addSingleRailObjectToQCircuit[circuit_, ass_Association] /; MatchQ[circuit, _QCircuit] := With[{
  newPointPos = findClosestPointOnLine[MousePosition["Graphics"], circuit]
},
  AppendTo[
    circuit[[1, "Gates"]],
    {
      newPointPos[[1]],
      <|
        ass,
        "Args" -> newPointPos[[2]]
      |>
    }
  ]
];


(* `addDoubleRailObjectToQCircuit` adds the correctly formatted element
    corresponding to a 2 qubit gate to the main `QCircuit` object.
    The name and other specs of the gate to be added are into the input
    argument `ass`, while `temporaryStuff` is needed as an input because
    it contains the location of the control point the was first added.

    Note that this function *does not* draw anything.
    It just handles the operation of adding the properly formatted
    element to the `QCircuit` object.
*)
Attributes[addDoubleRailObjectToQCircuit] = {HoldAll};
addDoubleRailObjectToQCircuit[
  circuit_, temporaryStuff_, ass_Association
] /; MatchQ[circuit, _QCircuit] := With[{
  newPointPos = pointOnLine[circuit]
},
  AppendTo[
    circuit[[1, "Gates"]],
    {
      temporaryStuff["2QubitGateInitialPoint"][[1]],
      <|
        ass,
        "Args" -> {
          temporaryStuff["2QubitGateInitialPoint"][[2]],
          newPointPos[[2]]
        }
      |>
    }
  ]
];


(* In eventHandling are defined all the actions to take when something
   on the graphics is clicked, in the various circumstances.

   `action` defines the current mode of operation, and is defined inside
       the DynamicModule created by `DrawQCircuit`.
   `temporaryStuff` is an association containing a list of objects
       describing stuff to be temporarily drawn onto the circuit, like
        for example the first point in a double rail circuit
*)
Attributes[eventHandling] = {HoldAll};
eventHandling[circuit_, action_, temporaryStuff_] /; MatchQ[circuit, _QCircuit] := {
  "MouseDown" :> Which[
  (* Add a nice black point *)
    TrueQ[action == "AddPoint"],
    addSingleRailObjectToQCircuit[circuit,
      <|"Type" -> "Debug", "Name" -> "NiceDot"|>
    ],
  (* Remove a 1 qubit gate, if in Add1QubitGate mode and if the ctrl
     modifier is being pressed *)
    And[
      MatchQ[action, {"Add1QubitGate", _String}],
      MemberQ[CurrentValue["ModifierKeys"], "Control"]
    ],
    circuit[[1, "Gates"]] = Delete[
      circuit[[1, "Gates"]],
      findGateCloserToMouse[circuit]
    ],
  (* Add a 1 qubit gate*)
    MatchQ[action, {"Add1QubitGate", _String}],
    addSingleRailObjectToQCircuit[circuit,
      <|"Type" -> "1QubitGate", "Name" -> action[[2]]|>
    ],
  (* Add 2 qubit gates. *)
    MatchQ[action, {"Add2QubitGate", _String}],
    action = {"Adding2QubitGate", action[[2]]};
    temporaryStuff["2QubitGateInitialPoint"] = findClosestPointOnLine[
      MousePosition["Graphics"], circuit
    ],
  (*  In the course of adding a 2 qubit gate.
      This action is activated when the first element of a 2 qubit
      gate has been added, but not the second one yet.
  *)
    MatchQ[action, {"Adding2QubitGate", _String}],
    With[{
      firstRail = temporaryStuff["2QubitGateInitialPoint"][[2]],
      secondRail = findClosestPointOnLine[MousePosition["Graphics"], circuit][[2]],
      xCoord = temporaryStuff["2QubitGateInitialPoint"][[2]]
    },
      If[firstRail != secondRail,
        addDoubleRailObjectToQCircuit[circuit, temporaryStuff,
          <|"Type" -> "2QubitGate", "Name" -> action[[2]]|>
        ];
        Unset[temporaryStuff["2QubitGateInitialPoint"]];
        action = {"Add2QubitGate", action[[2]]}
      ]
    ],
    True,
    Print["Nothing to be done, mate"]
  ]
};


optionsBarAvailableOneQubitGates = {
  "H", "X", "Y", "Z"
};
optionsBarAvailableTwoQubitGates = {
  "CPHASE",
  "CNOT"
};

optionsBar := Dynamic @ RadioButtonBar[Dynamic @ action, {
  (*"AddPoint" -> "Add point",*)
  Sequence @@ Table[
    {"Add1QubitGate", gateName} -> "Add " <> gateName,
    {gateName, optionsBarAvailableOneQubitGates}
  ],
(*  When adding a 2 qubit gate one has to first click on a point,
    and then click on a second point on a different rail.
    In between these two events, a temporary point must be printed to
    remember the position of the first qubit rail. *)
  Sequence @@ Table[
    If[action === {"Adding2QubitGate", gateName},
      action -> "Adding " <> gateName <> "...",
      {"Add2QubitGate", gateName} -> "Add " <> gateName
    ],
    {gateName, optionsBarAvailableTwoQubitGates}
  ]
},
  Enabled -> True,
  Appearance -> "Vertical"
];


(*  Draw the actual circuit.
    Basically all the rest of this file builds up the necessary components for this function.

    DynamicModule variables:
    -----------------------
    `action`: specifies the current mode.
        The typical values are:
          {"Add1QubitGate", "NameOfGate"},
          {"Add2QubitGate", "NameOfGate"},
          {"Adding2QubitGate", "NameOfGate"}
    `temporaryStuff`: contains temporary objects to be drawn.
        This includes the initially clicked point when adding a 2 qubit gate.

    Main function used:
    ------------------
    `drawRails`: draws the basic backbone of the circuit, which in practice
        comes down to the lines for the qubits (for now at least).
    `drawGates`: draws all the gates on the circuit.
        The gates displayd are all and only those specified in `circuit`,
        specifically in `circuit[[1, "Gates"]].
        This function draws both 1 qubit and 2 qubit gates, and anything else.
    `drawTemporaryStuff`: there may be additional elements to print on the
        graphics, like the initial point used to remember the control qubit
        when adding a 2 qubit gate.
        In general everything that is in this variable should only be
        temporary drawn in the graphics, but is not reflected by the
        actual content of the main `QCircuit` object.
*)
Attributes[DrawQCircuit] = {HoldAll};
DrawQCircuit[circuit_] /; MatchQ[circuit, _QCircuit] := DynamicModule[{
  action = {"Add2QubitGate", "CNOT"},
  temporaryStuff = Association[]
},
  Row[#, Spacer @ 20] & @ {
    Panel[optionsBar,
      ImageSize -> {150, Automatic}
    ],
    EventHandler[#, eventHandling[circuit, action, temporaryStuff]]&[
      Dynamic @ Graphics[{
        drawRails @ circuit,
        drawGates @ circuit,
        If[MousePosition["Graphics"] =!= None,
          drawTemporaryStuff[circuit, temporaryStuff, action]
        ]
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