(* ::Package:: *)

DirectoryOverwrite[dir1_,dir2_]:=(
	If[FileType[dir2]==Directory,
		DeleteDirectory[dir2,DeleteContents->True]
	];
	CopyDirectory[dir1,dir2];
)


(* ::Text:: *)
(*Install Packages*)


DirectoryOverwrite[
	FileNameJoin[{NotebookDirectory[],"NDArray"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","NDArray"}]
]
