#!/usr/bin/env wolframscript
(* ::Package:: *)

Unprotect[YRange, Dplot];

ClearAll[Dplot, YRange];

YRange = YRange;

Options[Dplot] = {ImageSize -> Full, YRange -> {Full, Full}};

Dplot[list_, opt : OptionsPattern[{Dplot, Plot}]] :=
    DateListPlot[list, Evaluate @ FilterRules[{opt}, Options[Plot]], 
        ImageSize -> OptionValue[ImageSize], PlotRange -> {{Automatic, FromDateString[
        "2021-12-14"]}, OptionValue[YRange]}];

Protect[Dplot, YRange];


RenderP := {e, name} |-> Module[{},
	Export["/Users/alejandro/out/"<>name<>".pdf", e];
	SystemOpen["/Users/alejandro/out/"<>name<>".pdf"];
];


AbsoluteTiming[
m = 0;
f = 0;

Do[
	file = "~/Downloads/covid/covid_"<>ToString@file<>".csv";
	Print["Loading... "<>file];
	data = Import[file, {"CSV", "Dataset"}, "HeaderLines"->1];
	sexo = Normal@data[All, "SEXO"];
	m += Count[sexo, 1];
	f += Count[sexo, 2],
{file, 1, 101}];

{m, f}
]


result = <||>
Do[
	file = "~/Downloads/covid/covid_"<>ToString@file<>".csv";
	Print["Loading... "<>file];
	data = Import[file, {"CSV", "Dataset"}, "HeaderLines"->1];
	fecha = Counts[Normal@data[All, "FECHA_INGRESO"]];
	result = Merge[{result, fecha}, Total],
{file, 1, 101}]

fecha = Flatten /@ List @@@ Normal @ result;
Do[fecha[[n]][[1]] = FromDateString[fecha[[n]][[1]]], {n, 1, Length[fecha]}];


result = <||>
Do[
	file = "~/Downloads/covid/covid_"<>ToString@file<>".csv";
	Print["Loading... "<>file];
	data = Import[file, {"CSV", "Dataset"}, "HeaderLines"->1];
	fecham = Counts[Normal@data[All, "FECHA_DEF"]];
	result = Merge[{result, fecham}, Total],
{file, 1, 101}]

fecham = Flatten /@ List @@@ Normal @ result;
Do[fecham[[n]][[1]] = FromDateString[fecham[[n]][[1]]], {n, 1, Length[fecham]}];


ResourceFunction["CombinePlots"][,DateListPlot[ fecham, PlotRange->{{Automatic, FromDateString["2021-12-14"]}, Automatic}, ImageSize->Full, PlotStyle->Darker[Red]] ]


ResourceFunction["CombinePlots"][
 DateListPlot[ fecha, PlotRange->{{Automatic, FromDateString["2021-12-14"]}, {-5, Automatic}}, ImageSize->Full, Frame->True],
 DateListPlot[ fecham, PlotRange->{{Automatic, FromDateString["2021-12-14"]}, {-5, 6000}}, ImageSize->Full, Frame->True, FrameStyle -> Darker@Red, PlotStyle -> Darker@Red],
 "AxesSides" -> "TwoY"
 ]


result = <||>
Do[
	file = "~/Downloads/covid/covid_"<>ToString@file<>".csv";
	Print["Loading... "<>file];
	data = Import[file, {"CSV", "Dataset"}, "HeaderLines"->1];
	edades = Counts[Normal@data[All, "EDAD"]];
	result = Merge[{result, edades}, Total],
{file, 1, 101}]

edades = Flatten /@ List @@@ Normal @ edades;





result = <||>
Do[
	file = "~/Downloads/covid/covid_"<>ToString@file<>".csv";
	Print["Loading... "<>file];
	data = Import[file, {"CSV", "Dataset"}, "HeaderLines"->1];
	dates = Counts[Normal@data[All, "FECHA_INGRESO"]];
	result = Merge[{result, dates}, Total],
{file, 1, 101}]v

dates = Flatten /@ List @@@ Normal @ result;
Do[dates[[n]][[1]] = FromDateString[dates[[n]][[1]]], {n, 1, Length[dates]}];

DateListPlot[dates, PlotRange->{{Automatic, FromDateString["2021-12-14"]}, Automatic}, ImageSize->Full]



result = <||>
Do[
	file = "~/Downloads/covid/covid_"<>ToString@file<>".csv";
	Print["Loading... "<>file];
	data = Import[file, {"CSV", "Dataset"}, "HeaderLines"->1];
	datesCovid = Counts[Normal@data[Select[#"CLASIFICACION_FINAL" == 3 &], "FECHA_INGRESO"] ];
	result = Merge[{result, datesCovid}, Total],
{file, 1, 101}]

datesCovid = Flatten /@ List @@@ Normal @ result;
Do[datesCovid[[n]][[1]] = FromDateString[datesCovid[[n]][[1]]], {n, 1, Length[datesCovid]}];

DateListPlot[datesCovid, PlotRange->{{Automatic, FromDateString["2021-12-14"]}, Automatic}, ImageSize->Full]



Show[Dplot[dates], Dplot[datesCovid, PlotStyle->Red]]


Sum[k[[2]],{k, datesCovid}]


Sum[k[[2]], {k, dates}]


PieChart[
	{{12133530-3663159, 3663159}}, 
	ChartLabels-> Placed[{
		ToString@(N[(12133530-3663159)/12133530*100])<>"%",
		ToString@(N[3663159/12133530*100])<>"%"
	},  "VerticalCallout"], 
	SectorSpacing->None,
	ImageSize->Full,
	ChartStyle->EdgeForm[None],
	ChartBaseStyle->EdgeForm[Dashed],
	LabelStyle->Directive[20]
]


rawdatesqc = {};
Do[AppendTo[rawdatesqc, e[[2]]],  {e, SortBy[datesCovid, AbsoluteTime[{#[[1]], {"Day", "Month", "Year"}}] &]}]
rawdatesqc = PrependTo[rawdatesqc, Table[0, 31+29]];
rawdatesqc = Flatten[rawdatesqc];
rawdatesqc


rawdatesq = {};
Do[AppendTo[rawdatesq, e[[2]]],  {e, SortBy[dates, AbsoluteTime[{#[[1]], {"Day", "Month", "Year"}}] &]}]
rawdatesq


ListLinePlot[{rawdatesq, rawdatesqc, percents*1000}]


SortBy[dates, AbsoluteTime[{#[[1]], {"Day", "Month", "Year"}}] &]
SortBy[datesCovid, AbsoluteTime[{#[[1]], {"Day", "Month", "Year"}}] &]


percentPerDate = {};
percents = N[rawdatesqc/rawdatesq * 100];
Do[AppendTo[percentPerDate, {SortBy[dates, AbsoluteTime[{#[[1]], {"Day", "Month", "Year"}}] &][[i]][[1]], percents[[i]]}], {i, 1, Length[rawdatesq]}]
percentPerDate


RenderP[Dplot[percentPerDate, YRange -> {0,100}, Filling -> Top], "Percent"]


Dplot[dates]


RenderP[Show[Dplot@dates, Dplot[datesCovid, PlotStyle-> Darker@Red]], "TotalVSConf"]


Show[Dplot@dates, Dplot[datesCovid, PlotStyle-> Darker@Red]]
