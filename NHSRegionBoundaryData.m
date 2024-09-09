importNHSRegionData[]:= Import["NHS_England_Regions_January_2024_EN_BGC_2411870618491533064.geojson", "Data"]

NHSRegionBoundaryData[geoData_, assoc_]:= Module[{polygonAssoc, NHSRegions, geometryAssoc, plotData},

polygonAssoc = <|(#ID -> #Geometry) & /@ geoData["Features"]|>;
NHSRegions = <|
1 -> "London", 
2 -> "South East", 
3 -> "South West", 
4 -> "Midlands", 
5 -> "North East and Yorkshire", 
6 -> "East of England", 
7 -> "North West"|>;
geometryAssoc = AssociationThread[Values[NHSRegions], Values[polygonAssoc]];
plotData = KeyValueMap[
   {name, geometry} |-> (
     GeoGroup[geometry] -> assoc[name]
     ),
   geometryAssoc
   ]

]

(* Example usage. *)

geoData = importNHSRegionData[];
sevenRegions = <|
   "East of England" -> 1,
   "Midlands" -> 2,
   "South West" -> 3,
   "South East" -> 4,
   "North East and Yorkshire" -> 5,
   "North West" -> 6,
   "London" -> 7
   |>;

GeoRegionValuePlot[
   NHSRegionBoundaryData[geoData, sevenRegions],
   GeoRange -> 
  Entity["AdministrativeDivision", {"England", "UnitedKingdom"}],
   PlotLegends -> BarLegend[Automatic, LegendLabel -> "Label"]
 ]
