FindFile["Fretica`"];
Needs["Fretica`"];


fSetupAnalysis[fileName_] := 
Module[{metadata, D1, D2, D12, activeLasers, channelsPerPulse, chsL1, chsL2, chsAll},
If[Check[FOpenTTTR[fileName], $Failed] === $Failed, Return[0]];

fGetStringFromHeader[s_String] := ToExpression[StringCases[FShowHeader[], s ~~ Whitespace ~~ ":" ~~ Whitespace ~~ v : WordCharacter .. -> v][[1]]];

(* Laser configuration. *)
metadata = Association[
"file" -> fileName,
"name" -> StringDelete[fileName, ".ptu"],
"485V" -> Boole@FGetFromHeader[{"Sep2_SOM_100_Seq_EnaOutput", 0}],
"485H" -> Boole@FGetFromHeader[{"Sep2_SOM_100_Seq_EnaOutput", 1}],
"560V" -> Boole@FGetFromHeader[{"Sep2_SOM_100_Seq_EnaOutput", 2}],
"640V" -> Boole@FGetFromHeader[{"Sep2_SOM_100_Seq_EnaOutput", 3}],
"640H" -> Boole@FGetFromHeader[{"Sep2_SOM_100_Seq_EnaOutput", 4}],

(*TCSPC configuration*)
(*FGetFromHeader extracts only integer part of BaseOscFreq. Mind when working with low frequencies!*)
"clockSyncRate" -> FGetFromHeader["Sep2_SOM_100_BaseOscFreq"]/FGetFromHeader["Sep2_SOM_100_Divider"],
"channelWidth" -> FGetFromHeader["HW_BaseResolution"],
"pulseCfg" -> fGetStringFromHeader["UsrPulseCfg"],
(*Dimensions*)
"dimensions" -> FGetFromHeader["ImgHdr_Dimensions"],
"UsrPowerDiode" -> FGetFromHeader["UsrPowerDiode"]
];

(* Routing and detector configuration. *)
(*Practically always both SPADs are used!*)
FSetActiveRoutes[{1, 1}];

If[metadata["pulseCfg"] == PIE,
FEnablePie[True];
FSetFRETRoutes[{"A", "D"}];
FSetPIERoutes[FGetAcceptorRoutes[]],
FEnablePie[False];
FSetFRETRoutes[{"A", "D"}]];

D1 = {1, 0};(*longer-wavelength emission*)
D2 = {0, 1};(*shorther-wavelength emission*)
D12 = {1, 1};(*All photons.*)

activeLasers = Total@Values[KeyTake[{"485V", "485H", "560V", "640V", "640H"}][metadata]];
channelsPerPulse = Round@(1/metadata["clockSyncRate"]/metadata["channelWidth"]);

chsL1 = {1, Round@channelsPerPulse};(*Microtime channels after laser 1 excitation.*)

If[metadata["pulseCfg"] == PIE,
chsL2 = {channelsPerPulse + 1, 2*channelsPerPulse};(*Microtime channels after laser 2 excitation.*)
chsAll = {1, activeLasers*channelsPerPulse};
FSetChannelConstraints[1, chsL1];
FSetChannelConstraints[2, chsL1];
FSetPIEChannelConstraints[1, chsL2];
FSetPIEChannelConstraints[2, chsL2];
];

AssociateTo[metadata, {
"D1" -> D1,
"D2" -> D2,
"D12" -> D12,
"activeLasers" -> activeLasers,
"channelsPerPulse" -> channelsPerPulse,
"chsL1" -> chsL1
}];

If[metadata["pulseCfg"] == PIE,
AssociateTo[metadata, {
"chsL2" -> chsL2,
"chsAll" -> chsAll
}]];

AssociateTo[metadata, fGetPhotonData[metadata]];

(* Image metadata *)
If[metadata["dimensions"] <= 1, Return[metadata]];

AssociateTo[metadata, {
"PixX" -> FGetFromHeader["ImgHdr_PixX"],
"PixY" -> FGetFromHeader["ImgHdr_PixY"],
"PixResol" -> FGetFromHeader["ImgHdr_PixResol"],
"MaxFrames" -> FGetFromHeader["ImgHdr_MaxFrames"],
"TimePerPixel" -> FGetFromHeader["ImgHdr_TimePerPixel"],
"BiDirect" -> FGetFromHeader["ImgHdr_BiDirect"]
}];

metadata
];


SetAttributes[fGetPhotonData, HoldFirst];
fGetPhotonData[metadata_] := 
Module[{startTime, stopTime, hD1, cD1, hD2, cD2, hD1L1, cD1L1, hD1L2, cD1L2, hD2L1, cD2L1, hD2L2, cD2L2, assSTD, assPIE, assoc},
If[metadata === 0, Return[0]];

(* Time information *)
startTime = FGetStartTime[]/1000.;(*start of record (?) in SECONDS*)
stopTime = FGetStopTime[]/1000.;(*duration of record (?) in SECONDS*)

(* Further photon characteristics *)
hD1 = FLifeTimeHisto[FRoutes -> {1}, FPhotonData -> All, FOutput -> FData][[1]];
cD1 = Total[hD1[[All, 2]]];
hD2 = FLifeTimeHisto[FRoutes -> {2}, FPhotonData -> All, FOutput -> FData][[1]];
cD2 = Total[hD2[[All, 2]]];

If[metadata["pulseCfg"] == PIE,
hD1L1 = Select[hD1, Between[#[[1]], metadata["chsL1"]*metadata["channelWidth"]*1*^9] &];
cD1L1 = Total[hD1L1[[All, 2]]];
hD1L2 = Select[hD1, Between[#[[1]], metadata["chsL2"]*metadata["channelWidth"]*1*^9] &];
cD1L2 = Total[hD1L2[[All, 2]]];

hD2L1 = Select[hD2, Between[#[[1]], metadata["chsL1"]*metadata["channelWidth"]*1*^9] &];
cD2L1 = Total[hD2L1[[All, 2]]];
hD2L2 = Select[hD2, Between[#[[1]], metadata["chsL2"]*metadata["channelWidth"]*1*^9] &];
cD2L2 = Total[hD2L2[[All, 2]]];
];

assSTD = Association[{
(*"hD1"->hD1,*)
"cD1" -> cD1,
"crD1" -> cD1/(stopTime - startTime),
(*"hD2"->hD2,*)
"cD2" -> cD2,
"crD2" -> cD2/(stopTime - startTime),
"crD2L2" -> cD2L2/(stopTime - startTime),
"startTime" -> startTime,
"stopTime" -> stopTime,
"time" -> stopTime - startTime}];

If[metadata["pulseCfg"] == PIE,
assPIE = Association[{
(*"hD1L1"->hD1L1,*)
"cD1L1" -> cD1L1,
"crD1L1" -> cD1L1/(stopTime - startTime),
(*"hD1L2"->hD1L2,*)
"cD1L2" -> cD1L2,
"crD1L2" -> cD1L2/(stopTime - startTime),
(*"hD2L1"->hD2L1,*)
"cD2L1" -> cD2L1,
"crD2L1" -> cD2L1/(stopTime - startTime),
(*"hD2L2"->hD2L2,*)
"cD2L2" -> cD2L2}];
];

If[metadata["pulseCfg"] == PIE,
assoc = Join[{assSTD, assPIE}]];

If[metadata["pulseCfg"] == Standard,
assoc = assSTD];

assoc
];


SetAttributes[fGetCorrData, HoldFirst];
fGetCorrData[metadata_, log10taumin_, interval_, correlation_] := fGetCorrData[metadata, log10taumin, 0., interval, correlation];
fGetCorrData[metadata_, log10taumin_, log10taumax_, interval_, correlation_] := 
Module[{startTime, stopTime, chs, num, weights, wl1, wl2, wl12, D1, D2, D12, chsL1, chsL2, chsAll, intervals, corrdata, CCtotal, CCinterval, meanCCinterval, SD, SEM},
If[metadata === 0, Return[0]];
If[StringQ[metadata["corr"]], KeyDrop[metadata, {metadata["corr"], "corr", "CCtotal", "mean", "SD", "SEM"}]];

(*Update data on counts, countrates, etc. after cutting.*)
AssociateTo[metadata, fGetPhotonData[metadata]];

(*Lets not autocorrelate images.*)
If[metadata["dimensions"] > 1, Print["This is not a time trace."]; Return[0]];

startTime = metadata["startTime"];
stopTime = metadata["stopTime"];

(* calculating correlation data *)
chs = metadata["chsL1"][[2]];
num[number_] := number & /@ Range[chs];

D1 = metadata["D1"];
D2 = metadata["D2"];
D12 = metadata["D12"];

If[metadata["pulseCfg"] == PIE,
chsL1 = metadata["chsL1"];
chsL2 = metadata["chsL2"];
chsAll = metadata["chsAll"];
];

AssociateTo[metadata, {
"log10taumin" -> log10taumin,
"log10taumax" -> log10taumax}];

If[metadata["pulseCfg"] == Standard,
chsAll = metadata["chsL1"]];

fGetCCFFCS[route1_, route2_, chsroute1_, chsroute2_] :=
Module[{CCtot, CCint},
CCtot = FFCS[log10taumin, log10taumax, CORRSTEPSIZE, route1, route2, chsroute1, chsroute2, FPhotonData -> All, FOutput -> FData];
CCint = FFCS[log10taumin, log10taumax, CORRSTEPSIZE, route1, route2, chsroute1, chsroute2, #, # + interval, FPhotonData -> All, FOutput -> FData] & /@ Range[startTime, stopTime - interval, interval];
{CCtot, CCint}
];

If[correlation == "AC2111FFCS", AC2111FFCS = fGetCCFFCS[D2, D1, chsL1, chsL1];];
If[correlation == "AC2122FFCS", AC2122FFCS = fGetCCFFCS[D2, D1, chsL2, chsL2];];
If[correlation == "AC1211FFCS", AC1211FFCS = fGetCCFFCS[D1, D2, chsL1, chsL1];];
If[correlation == "AC1222FFCS", AC1222FFCS = fGetCCFFCS[D1, D2, chsL2, chsL2];];
If[correlation == "AC12FFCS", AC12FFCS = fGetCCFFCS[D1, D2, chsAll, chsAll];];
If[correlation == "AC21FFCS", AC21FFCS = fGetCCFFCS[D2, D1, chsAll, chsAll];];
If[correlation == "AC2211FFCS", AC2211FFCS = fGetCCFFCS[D2, D2, chsL1, chsL1];];
If[correlation == "AC1122FFCS", AC1122FFCS = fGetCCFFCS[D1, D1, chsL2, chsL2];];
If[correlation == "CC1221FFCS", CC1221FFCS = fGetCCFFCS[D1, D2, chsL2, chsL1];];
If[correlation == "CC2112FFCS", CC2112FFCS = fGetCCFFCS[D2, D1, chsL1, chsL2];];

corrdata = Symbol[correlation];
CCtotal = corrdata[[1]];
CCtotal = Transpose[{CCtotal[[All, 1]]*1.*^6, CCtotal[[All, 2]] - 1.}];

CCinterval = corrdata[[2]];
intervals = Length@CCinterval;
CCinterval = Transpose[{CCinterval[[All, All, 1]][[#]]*1.*^6, CCinterval[[All, All, 2]][[#]] - 1.}] & /@ Range@intervals;
meanCCinterval = Mean@CCinterval;
SD = StandardDeviation[CCinterval][[All, 2]];
SEM = SD/Sqrt[intervals];

AssociateTo[metadata,
{"corr" -> correlation,
"CCtotal" -> CCtotal,
correlation -> CCinterval,
"mean" -> meanCCinterval,
"SD" -> SD,
"SEM" -> SEM,
"intervals" -> intervals}]
];


SetAttributes[{fFitCorrDataWeighted, fFitCorrDataUnweighted}, HoldFirst];
fFitCorrDataWeighted[metadata_, kappa_Real, fitModel_String] := fFitCorrDataWeighted[metadata, kappa, fitModel, True];
fFitCorrDataUnweighted[metadata_, kappa_Real, fitModel_String] := fFitCorrDataWeighted[metadata, kappa, fitModel, False];
fFitCorrDataWeighted[metadata_, kappa_Real, fitModel_String, weighting_] :=
Module[{data, SEM, model, parameters, cons1, cons2, guess1, guess2, selection},
If[metadata === 0, Return[0]];
KeyDrop[metadata, {"fitmodel", "corrfit", "fitparams"}];

data = metadata["CCtotal"];
SEM = metadata["SEM"];
If[weighting,
Nothing,
SEM = (1 & /@ Range@Length@SEM)];
SP = kappa;

model = cfmM[fitModel];
parameters = cfmP[fitModel];
cons1 = cfmC[fitModel];
cons2 = {};
Map[Function[param,
selection = Select[cons1, param == #[[1]] &][[1]];
AppendTo[cons2, (selection[[2]] < selection[[1]] < selection[[3]])]], parameters];
guess1 = cfmPG[fitModel];
guess2 = {#[[1]], #[[2]]} & /@ guess1;

fitdata = NonlinearModelFit[data,
Join[{model}, cons2],
guess2,
{t},
Weights -> 1/SEM,
MaxIterations -> 10000, PrecisionGoal -> Infinity];
params = fitdata["BestFitParameters"];
fitparams = Association[params];

AssociateTo[metadata, {
"fitmodel" -> fitModel,
"corrfit" -> fitdata,
"fitparams" -> fitparams
}];

ClearAll[SP, fitdata, params, fitparams];
];


(* 
Correlation fit models, parameters, constraints and parameter guesses
P. Schwille, in Fluorescence Correlation Spectroscopy, F. P. Sch\[ADoubleDot]fer, J. P. Toennies, W. Zinth, R. Rigler, E. S. Elson, Eds. (Springer Berlin Heidelberg, Berlin, Heidelberg, 2001), vol. 65, pp.\[NonBreakingSpace]360-378.
S. T. Hess, S. Huang, A. A. Heikal, W. W. Webb, Biological and chemical applications of fluorescence correlation spectroscopy: a review. Biochemistry. 41, 697-705 (2002).
S. T. Hess, W. W. Webb, Focal volume optics and experimental artifacts in confocal fluorescence correlation spectroscopy. Biophys J. 83, 2300-2317 (2002).
*)
HessWebbD1T1 = (G1/((1 + t/tauD1) Sqrt[1 + t/(SP^2 tauD1)])) (1 + T1/(1 - T1) E^(-(t/tauT1))) + y0;
HessWebbD1T2 = (G1/((1 + t/tauD1) Sqrt[1 + t/(SP^2 tauD1)])) (1 + T1/(1 - T1) E^(-(t/tauT1))) (1 + T2/(1 - T2) E^(-(t/tauT2))) + y0;
HessWebbD2T1 = ( G1/((1 + t/tauD1) Sqrt[1 + t/(SP^2 tauD1)]) + G2/((1 + t/tauD2) Sqrt[1 + t/(SP^2 tauD2)])) (1 + T1/(1 - T1) E^(-(t/tauT1))) + y0;
HessWebbD3T1 = ( G1/((1 + t/tauD1) Sqrt[1 + t/(SP^2 tauD1)]) + G2/((1 + t/tauD2) Sqrt[1 + t/(SP^2 tauD2)]) + G3/((1 + t/tauD3) Sqrt[1 + t/(SP^2 tauD3)])) (1 + T1/(1 - T1) E^(-(t/tauT1))) + y0;
HessWebbD2T2 = (G1/((1 + t/tauD1) Sqrt[1 + t/(SP^2 tauD1)]) + G2/((1 + t/tauD2) Sqrt[1 + t/(SP^2 tauD2)])) (1 + T1/(1 - T1) E^(-(t/tauT1))) (1 + T2/(1 - T2) E^(-(t/tauT2))) + y0;
HessWebb2D1T1 = G1/((1 + t/tauD1)) (1 + T1/(1 - T1) E^(-(t/tauT1))) + y0;

ClearAll[SP, calSP, cfmM];
cfmM = Association[
"cal3D1T2" -> HessWebbD2T2 /. {G2 -> 0, SP -> calSP},
"cal3D1T1" -> HessWebbD1T1 /. {SP -> calSP},
"cal3D1T0" -> HessWebbD1T1 /. {SP -> calSP, T1 -> 0},
"3D1T1" -> HessWebbD1T1,
"3D1T2" -> HessWebbD1T2,
"3D2T1" -> HessWebbD2T1,
"3D2T2" -> HessWebbD2T2,
"3D3T1" -> HessWebbD3T1,
"3D1T0" -> HessWebbD1T1 /. {T1 -> 0},
"3D2T0" -> HessWebbD2T1 /. {T1 -> 0},
"2D1T1" -> HessWebb2D1T1,
"2D1T0" -> HessWebb2D1T1 /. {T1 -> 0}
];

getSymbols[model_] := DeleteElements[Union@Cases[model, _Symbol?(! NumericQ[#] &), Infinity], {t, SP}];
cfmP = Association[# -> getSymbols[cfmM[#]] & /@ Keys[cfmM]];

defaultconstraints = Association[
"G1" -> {G1, 0, 20},
"G2" -> {G2, 0, 20},
"G3" -> {G3, 0, 20},
"tauD1" -> {tauD1, 0, 15000},
"tauD2" -> {tauD2, 0, 15000},
"tauD3" -> {tauD3, 0, 20000},
"T1" -> {T1, 0.01, 1},
"tauT1" -> {tauT1, 1, 150},
"T2" -> {T2, 0.01, 1},
"tauT2" -> {tauT2, 1, 150},
"y0" -> {y0, -0.1, 0.5},
"calSP" -> {calSP, 5, 20}
];

cfmC = Association[
Map[Function[iPerModel,
iPerModel -> Map[Function[iPerParam,
defaultconstraints[ToString[cfmP[iPerModel][[iPerParam]]]]],
Range@Length@cfmP[iPerModel]]],
Keys[cfmP]]
];

defaultguesses = Association[
"G1" -> {G1 -> 1.},
"G2" -> {G2 -> 1.},
"G3" -> {G3 -> 1.},
"tauD1" -> {tauD1 -> 100.},
"tauD2" -> {tauD2 -> 1000.},
"tauD3" -> {tauD3 -> 2000.},
"T1" -> {T1 -> 0.1},
"tauT1" -> {tauT1 -> 10.},
"T2" -> {T2 -> 0.1},
"tauT2" -> {tauT2 -> 10.},
"y0" -> {y0 -> 0.},
"calSP" -> {calSP -> 5.}
];

cfmPG = Association[
Map[Function[iPerModel,
iPerModel -> Flatten@Map[Function[iPerParam,
defaultguesses[ToString[cfmP[iPerModel][[iPerParam]]]]],
Range@Length@cfmP[iPerModel]]],
Keys[cfmP]]
];


SetAttributes[fShowCorrPlotsWithFit,HoldFirst];(* to be able to AssociateTo corrdata to metadata *)
fShowCorrPlotsWithFit[metadata_]:=
Module[{traces, fitModel, corrdat, error, corrdatwitherrors, plotStyles, plotDataAndFit, plotFitResiduals, params, fitParams, grids},
If[metadata === 0, Return[0]];

traces = metadata[metadata["corr"]];
fitModel = metadata["corrfit"];

corrdat = fitModel["Data"];
error = metadata["SEM"];
corrdatwitherrors = Transpose[{corrdat[[All,1]],Around[#[[1]],#[[2]]]&/@Transpose[{corrdat[[All,2]],error}]}];

plotStyles = {Frame -> True, FrameLabel -> {"\[Tau] (\[Micro]s)", "G(\[Tau])"}};
plotDataAndFit = Show[
ListLogLinearPlot[traces,
PlotStyle -> {Gray}, PlotRange -> {All, {-0.02, Full}}, ImageSize -> 300, AspectRatio -> 1, LabelStyle -> {Black, 14}, plotStyles],
ListLogLinearPlot[corrdatwitherrors,
PlotStyle -> {Blue}, plotStyles, PlotRange -> {All, {-0.02, Full}}, ImageSize -> 300, AspectRatio -> 1, LabelStyle -> {Black, 14}, IntervalMarkers -> "Bars", IntervalMarkersStyle -> Gray],
ListLogLinearPlot[Transpose[{corrdat[[All,1]],fitModel["Function"][corrdat[[All,1]]]}],
PlotStyle->{Red},Joined->True,plotStyles,PlotRange->{All,{-0.02,Full}},ImageSize->300,AspectRatio->1,LabelStyle->{Black,14}], PlotRange -> All];

plotFitResiduals =
ListLogLinearPlot[Transpose[{corrdat[[All, 1]], fitModel["FitResiduals"]}],
PlotStyle -> {Black}, Joined -> True, plotStyles, LabelStyle -> {Black, 10}, ImageSize -> 200, PlotRange -> All];

params = Association[(fitModel["BestFitParameters"])];
fitParams = Association[params];

grids = Grid[{Keys@fitParams, Values[fitParams]}\[Transpose],
Frame -> All, Background -> {{LightGray, Yellow}, None}];

Row[{plotDataAndFit, Column[{grids, plotFitResiduals}]}, Frame -> True]
];


SetAttributes[fGetVolume, HoldFirst];
fGetVolume[metadata_, Di_]:=
Module[{SP, w0, Veff, result},
If[metadata === 0, Return[0]];
ClearAll[tauD1];
tauD1 = metadata["fitparams"][tauD1];
SP = KeySelect[metadata["fitparams"], StringContainsQ[ToString@#,"SP"]&][[1]];(*rather weird workaround because ... required due to the variable calSP in cal3D1tT1t*)
w0 = (Sqrt[4*(Di*1*^-12)*(tauD1*1*^-6)])*1*^9//N;(*nm*)
Veff = (Pi^(3/2)*SP*w0^3)/1*^9//N;(*fL*)
result = {"tauD1" -> tauD1, "D" -> Di, "SP" -> SP, "w0" -> w0, "Veff" -> Veff};
ClearAll[tauD1];
AssociateTo[metadata, {"calVol" -> result, "vol" -> "Veff"/.result}]; result
];


SetAttributes[fGetConcentration, HoldFirst];
fGetConcentration[metadata_, cal_]:=
Module[{n1, Veff, c1},
If[metadata === 0, Return[0]];
ClearAll[G1];
n1 = 1/(G1 /. metadata["fitparams"]);
Veff = cal["vol"];
c1 = n1/Veff/6.022*^23*1.*^15;
AssociateTo[metadata, {"N1" -> n1, "c1" -> c1, "G1" -> 1/n1}]; 
{n1, c1}
];


SetAttributes[fGetBrightness, HoldFirst];
fGetBrightness[assSample_, assBG_, channel_]:=
Module[{brightness},
If[metadata === 0, Return[0]];
(*This function disregards bleed-through photons.*)
brightness = (assSample[channel] - assBG[channel])/(assSample["N1"]);
AssociateTo[assSample, {"cpm" -> brightness}]; brightness
];


SetAttributes[fGetD, HoldFirst];
fGetD[metadata_, cal_]:=
Module[{tauDs, Ds},
If[metadata === 0, Return[0]];
ClearAll[D1];
tauDs = KeySelect[metadata["fitparams"], StringContainsQ[ToString@#,"tauD"]&];
Ds = KeyValueMap[StringDelete[ToString@#1,"tau"] -> ("w0"/.cal["calVol"])^2/(4*#2)&, tauDs];
AssociateTo[metadata, Ds];
Ds
];


(*PicoQuant application note: Absolute Diffusion Coefficients: Compilation of Reference Data for FCS Calibration*)
ViscosityOfWater[T_(*temperature in Kelvin*)] := 2.414*^-5*10^(247.8/(T - 140));
StokesEinstein[T_(*temperature in Kelvin*), shape_(*6*Pi for sphere; 12 for disk*), r_(*hydrodynamic radius*)] := (1.38065*10^-23*T)/(shape*ViscosityOfWater[T]*r);
DatT[Di_(*diffusion coefficient at 298.15 K*), T_(*diffusion coefficient at T*)] := Di*(T/298.15)*(ViscosityOfWater[298.15]/ViscosityOfWater[T]);


fApplyThreshold[metadata_, route_, t1_, bin_, r_, thr_] := 
Module[{n, dn, dnints, intervals},
n = FTimeTrace[route, bin, {0, t1}, FOutput -> FData];
dn = Select[n, #[[2]] > thr &];
dnints = {dn[[#, 1]] - (r*bin/1000), dn[[#, 1]] + (r*bin/1000)} & /@ Range[Length[dn]];
intervals = List @@ Interval @@ dnints;
FDeleteTTTRTimeInterval[intervals, FCloseGap -> True];
FTimeTrace[route, bin, {0, t1}, FOutput -> FGraph, PlotTheme -> "Detailed"]
];


fImages[fileName_, images_List] := 
Module[{metadata, imgs, commonMatrixStyle, row, string, imgD1, imgD2, imgL1D2, imgL2D1, imgL2D2, imgL1D1},
metadata = fSetupAnalysis[fileName];
If[metadata == 0, Return[0]];
(*Are we looking at an image, i.e. a multidimensional object?*)
If[metadata["dimensions"] <= 1, Return[]];

imgs = {};
commonMatrixStyle = {LabelStyle -> Black, ImageSize -> 100*72/25.4, ColorFunction -> Hue, PlotLegends -> Automatic};
If[MemberQ[images, "D2"], AppendTo[imgs, MatrixPlot[FGetFromPiezoScan["n2"+"n2pie", 1], PlotLabel -> Style["D2", {14, Blue}], commonMatrixStyle]]];
If[MemberQ[images, "D1"], AppendTo[imgs, MatrixPlot[FGetFromPiezoScan["n1"+"n1pie", 1], PlotLabel -> Style["D1", {14, Red}], commonMatrixStyle]]];
If[MemberQ[images, "L1D2"], AppendTo[imgs, MatrixPlot[FGetFromPiezoScan["n2", 1], PlotLabel -> Style["L1D2", {14, Blue}], commonMatrixStyle]]];
If[MemberQ[images, "L2D1"], AppendTo[imgs, MatrixPlot[FGetFromPiezoScan["n1pie", 1], PlotLabel -> Style["L2D1", {14, Red}], commonMatrixStyle]]];
If[MemberQ[images, "L2D2"], AppendTo[imgs, MatrixPlot[FGetFromPiezoScan["n2pie", 1], PlotLabel -> Style["L2D2", {14, Blue}], commonMatrixStyle]]];
If[MemberQ[images, "L1D1"], AppendTo[imgs, MatrixPlot[FGetFromPiezoScan["n1", 1], PlotLabel -> Style["L1D1", {14, Red}], commonMatrixStyle]]];

string = "File: " <> fileName <> "; active lasers: " <> StringRiffle[Keys@Select[KeyTake[{"485V", "485H", "560V", "640V", "640H"}][metadata], # == 1 &], ", "] <> "; Dimensions: " <> ToString@metadata["PixX"] <> " px \[Times]" <> ToString@metadata["PixY"] <> " px; resolution: " <> ToString@Round[metadata["PixResol"], 0.001] <> " \[Micro]m/px; Dimensions: " <> ToString@Round[metadata["PixX"]*metadata["PixResol"], 0.1] <> " \[Micro]m \[Times]" <> ToString@Round[metadata["PixY"]*metadata["PixResol"], 0.1] <> " \[Micro]m; dwell time: " <> ToString[metadata["TimePerPixel"]*1000] <> " \[Micro]s";
Print@string;

Row[imgs, "  ", Frame -> True]
];