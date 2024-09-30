(* Go to this site: https://geoportal.statistics.gov.uk/datasets/ons::integrated-care-boards-april-2023-en-bfe-2/explore *)
(* Click "Download" and download as a .geojson file. *)

importNHSICBData[] := Import["Integrated_Care_Boards_April_2023_EN_BFE_-5609496246791735642.geojson", "Data"];

createICBBoundaryAssoc[data_] := Module[{features, polygonList, ICBNames}, 
features = Values[data[[3]]];
polygonList = (Association /@ features)[[All, "Geometry"]];
ICBNames = Association[#["Properties"]]["ICB23NM"] & /@ (Association /@ features);
AssociationThread[ICBNames, polygonList]
]


(* Example Usage. *)
nhsBlackCountryPolygon = createICBBoundaryAssoc[importNHSICBData[]][[40]];
GeoGraphics[{EdgeForm[{Thick, Black}], FaceForm[None], nhsBlackCountryPolygon}]