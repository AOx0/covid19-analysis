#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Mexico's COVID19 DB analysis*)


(* ::Text:: *)
(*Custom fuctions and types definition for easier plotting:*)


Unprotect[YRange, XRange, Dplot, RenderP];

ClearAll[Dplot, YRange, XRange, RenderP];

YRange = YRange;
XRange = XRange;

Options[Dplot] = {ImageSize -> Full, YRange -> {Full, Full}, XRange -> {Automatic, FromDateString[
        "2021-12-14"]}};

Dplot[list_, opt : OptionsPattern[{Dplot, Plot}]] :=
    DateListPlot[list, Evaluate @ FilterRules[{opt}, Options[Plot]], 
        ImageSize -> OptionValue[ImageSize], PlotRange -> {OptionValue[XRange], OptionValue[YRange]}];

Protect[Dplot, YRange, XRange];

RenderP := {e, name} |-> Module[{},
	Export["/Users/alejandro/out/"<>name<>".pdf", e, "PDF", "AllowRasterization"->False];
	SystemOpen["/Users/alejandro/out/"<>name<>".pdf"];
];

Protect[RenderP];


(* ::Text:: *)
(*Custom functions to plot pies easier*)


Unprotect[PieFromTotal];
ClearAll[PieFromTotal];
PieFromTotal[total_, values_, legends_, opt: OptionsPattern[{PieChart}]] := Module[{p, j, o},
	p = {ToString@(N[(total-Sum[k, {k, values}])/total*100])<>"%"};
	For[j = 1, j <= Length@values, j++,
		p = AppendTo[p,  ToString@(N[values[[j]]/total*100])<>"%"]
	];
	
	o = {total-Sum[k, {k, values}]};
	For[j = 1, j <= Length@values, j++,
		o = AppendTo[o, values[[j]]]
	];
	
	PieChart[
		{o}, 
		ChartLabels-> Placed[p,  "VerticalCallout"], 
		ChartLegends->Placed[legends, Below],
		SectorSpacing->None,
		ChartStyle->EdgeForm[None],
		ImageSize->OptionValue[ImageSize],
		ChartBaseStyle->EdgeForm[Dashed],
		LabelStyle->Directive[20],
		PlotRange->All,
		SectorOrigin->OptionValue@SectorOrigin
	]
]

Protect[PieFromTotal];


(* ::Text:: *)
(*Custom fuctions and types definition for easier file manipulation:*)


Unprotect[Pos, Pre, Code, PerformForEach, StartF, EndF];
ClearAll[Pos, Pre, Code, PerformForEach, StartF, EndF];

Pos = Pos; Pre = Pre; Code = Code; StartF = StartF; EndF = EndF;
Options[PerformForEach] = {StartF -> 1, EndF -> Default};

PerformForEach[p_, code_, OptionsPattern[]] := Module[{path = p, f, file, data, n, name, end, start},
	path = StringReplace[path, "~" -> $HomeDirectory];
	n = StringCount[RunProcess[StringSplit["ls "<>path], "StandardOutput"], "\n"];
	name = Last@StringSplit[path, "/"]<>"_";
	path = If[Last@StringSplit[path, ""] == "/", path, path<>"/"];
	end = OptionValue[EndF] /. Default -> n;
	start = OptionValue[StartF];
	
	If[KeyExistsQ[code, Pre], code[Pre][]];
	Monitor[For[f = start, f <= end /. Default -> n, f++,
		file = path<>name<>ToString@f<>".csv";
		If[KeyExistsQ[code, Code], code[Code][<|"Data" -> Import[file, {"CSV", "Dataset"}, "HeaderLines"->1], "File"-> f|>]];
	], ProgressIndicator[f, {start, end+1}]];
	If[KeyExistsQ[code, Pos], code[Pos][]];
];

Protect[Pos, Pre, Code, PerformForEach, StartF, EndF];


PerformForEach["~/Downloads/covid", <|
	Pre -> ((
		result = <||>;
	)&),
	Code -> ((
		dates = Counts[Normal@#Data[All, "FECHA_INGRESO"]];
		result = Merge[{result, dates}, Total];
	)&),
	Pos -> ((
		dates = Flatten /@ List @@@ Normal @ result;
		Do[dates[[n]][[1]] = FromDateString[dates[[n]][[1]]], {n, 1, Length[dates]}];
		Print@Dplot[dates];
	)&)
|>];


PerformForEach["~/Downloads/covid", <|
	Pre -> ((
		result = <||>;
	)&),
	Code -> ((
		datesCovid = Counts[Normal@#Data[Select[#"CLASIFICACION_FINAL" == 3 &], "FECHA_INGRESO"] ];
		result = Merge[{result, datesCovid}, Total];
	)&),
	Pos -> ((
		datesCovid = Flatten /@ List @@@ Normal @ result;
		Do[datesCovid[[n]][[1]] = FromDateString[datesCovid[[n]][[1]]], {n, 1, Length[datesCovid]}];
		Print@Dplot[datesCovid];
	)&)
|>];


PieFromTotal[Sum[k[[2]], {k, dates}], {Sum[k[[2]],{k, datesCovid}]}]


rawdatesqc = {};
Do[AppendTo[rawdatesqc, e[[2]]],  {e, SortBy[datesCovid, AbsoluteTime[#[[1]]] &]}];
rawdatesqc = PrependTo[rawdatesqc, Table[0, 31+29]];
rawdatesqc = Flatten[rawdatesqc];

rawdatesq = {};
Do[AppendTo[rawdatesq, e[[2]]],  {e, SortBy[dates, AbsoluteTime[#[[1]]] &]}];

percentPerDate = {};
percents = N[rawdatesqc/rawdatesq * 100];
Do[percentPerDate = AppendTo[percentPerDate, {SortBy[dates, AbsoluteTime[#[[1]]] &][[i]][[1]], percents[[i]]}], {i, 1, Length[rawdatesq]}];

Dplot[percentPerDate, Filling -> Top, YRange->{0,100}]


RenderP[Dplot[percentPerDate, Filling -> Top], "Percent"]


RenderP[Show[Dplot@dates, Dplot[datesCovid, PlotStyle-> Darker@Red]], "TotalVSConf"]


Show[Dplot@dates, Dplot[datesCovid, PlotStyle-> Darker@Red]]


PerformForEach["~/Downloads/covid", <|
	Pre -> ((
		result = <||>;
	)&),
	Code -> ((
		Do[
			entity = Counts[Normal@#Data[Select[#"CLASIFICACION_FINAL" == 3 && #"ENTIDAD_UM" == en &], "FECHA_INGRESO"] ];
			If [KeyExistsQ[result, en], result[en] = Merge[{result[en], entity}, Total], result[en] = entity];
		,{en, 1, 32}]
	)&),
	Pos -> ((
	entity = <||>;
	plots = {};
	Do[
		entity[en] = Flatten /@ List @@@ Normal @ result[en];
		Do[entity[en][[n]][[1]] = FromDateString[entity[en][[n]][[1]]], {n, 1, Length[entity[en]]}];
		plots = AppendTo[plots, Dplot[entity[en]]];
	,{en, 1, 32}]
	)&)
|>];


estados = {"Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de M\[EAcute]xico", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
"Jalisco", "Estado de M\[EAcute]xico", "Michoac\[AAcute]n", "Morelos", "Nayarit", "Nuevo Le\[OAcute]n", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potos\[IAcute]", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas",
"Tlaxcala", "Veracruz", "Yucat\[AAcute]n", "Zacatecas"};

PieFromTotal[Sum[k, {k, Table[Sum[e[[2]], {e, entity[en]}], {en, 1, 32}]}],  Table[Sum[e[[2]], {e, entity[en]}], {en, 1, 32}], estados, ImageSize->Full,  SectorOrigin -> {{Pi/12, "Counterclockwise"}, 1}]


RenderP[RectangleChart[{Table[{10, Sum[e[[2]], {e, entity[en]}]}, {en, 1, 32}]}, ChartLabels-> Placed[estados, Above] , ImageSize->Scaled[2.3], LabelStyle->Directive[20]], "TodosEstadosRect"]
