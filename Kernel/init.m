(* Try to automatically download and install MaTeX
   if not already present in the system *)
If[
  FindFile["MaTeX`"] == $Failed,
  Print["The MaTeX package is missing, trying to install it..."];
  Module[{json, download, target},
    Check[
      json = Import["https://api.github.com/repos/szhorvat/MaTeX/releases/latest", "JSON"];
      download = Lookup[First@Lookup[json, "assets"], "browser_download_url"];
      target = FileNameJoin[{CreateDirectory[], "MaTeX.paclet"}];
      If[$Notebooks,
        PrintTemporary@Labeled[ProgressIndicator[Appearance -> "Necklace"], "Downloading...", Right],
        Print["Downloading..."]
      ];
      URLSave[download, target],
      Return[$Failed]
    ];
    If[FileExistsQ[target],
      PacletInstall[target];
      Print["Installed! You are good to go."],
      $Failed
    ]
  ]
];


Get["QM`QM`"];
Get["QM`QGates`"];
Get["QM`QCircuitDrawer`"];
Get["QM`ReckDecomposition`"];
Get["QM`BlochSphere`"];
