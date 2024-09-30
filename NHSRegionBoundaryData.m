(* Download data as a .geojson file from this site: 
https://www.data.gov.uk/dataset/3664f211-2655-45e1-9a9b-e21d5b76ba6c/nhs-england-regions-january-2024-en-bgc*)

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
regionData = <|
   "East of England" -> RandomReal[],
   "Midlands" -> RandomReal[],
   "South West" -> RandomReal[],
   "South East" -> RandomReal[],
   "North East and Yorkshire" -> RandomReal[],
   "North West" -> RandomReal[],
   "London" -> RandomReal[]
   |>;

GeoRegionValuePlot[
   NHSRegionBoundaryData[geoData, regionData],
   GeoRange -> 
  Entity["AdministrativeDivision", {"England", "UnitedKingdom"}],
   PlotLegends -> BarLegend[Automatic, LegendLabel -> "Label"]
 ]
