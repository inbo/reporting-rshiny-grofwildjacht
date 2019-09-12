
In this folder, the required data is collected to create the Rshiny application.

## reporting data files

The `rshiny_reporting_data_*.csv` are providing the raw data and numbers of the grofwild reporting, which will be updated when new data is available. 

Column metadata of the `rshiny_reporting_data_*.csv`- files is as follows:

| Kolom                        | Eigenschappen | Toelichting                              |
| ---------------------------- | ------------- | ---------------------------------------- |
| ID                           | int           | random ID                                |
| wildsoort                    | factor        | het type wildsoort                       |
| afschotjaar                  | int           | het jaar van afschot (2002-2019, of NA)  |
| afschot_datum                | date          | datum van afschot  (YYYY-MM-DD) or (DD-MM-YYYY)                      |
| postcode_afschot_locatie     | int           | de postcode van het afschot              |
| gemeente_afschot_locatie     | factor        | de gemeente van het afschot              |
| provincie                    | factor        | de provincie van het afschot (Voeren afzonderlijk van Limburg behalve voor ree) |
| geslacht.MF                  | factor        | het geslacht zoals weergegeven op het meldingsformulier |
| leeftijdscategorie_MF        | factor        | de leeftijdscategorie zoals weergegeven op het meldingsformulier |
| ontweid_gewicht              | int           | het ontweid of leeggewicht (gewicht zonder ingewanden) |
| aantal_embryos_onbekend      | boolean           | aanduiding waar of niet of het aantal embryo's gekend is of niet |
| aantal_embryos               | int           | het aantal embryo's                      |
| onderkaaklengte_links        | int           | de onderkaaklengte van de linker kaak gemeten door de jagers |
| onderkaaklengte_rechts       | int           | de onderkaaklengte van de rechter kaak gemeten door de jagers |
| doodsoorzaak                 | factor        | de doodsoorzaak van het dier             |
| leeftijd_maanden             | int           | de leeftijdsbepaling in maanden door het INBO |
| lengte_mm                    | int           | de onderkaaklengte gemeten door het INBO |
| Leeftijdscategorie_onderkaak | factor        | de leeftijdscategorie door het INBO      |
| onderkaaklengte_comp         | int           | compilatie van de onderkaaklengte: INBO indien gekend, anders jager      |
| leeftijd_comp | factor        | compilatie voor de leeftijdscategorie: INBO indien gekend, anders jager      |


## GIS data files

The `*.geojson` files are providing GIS data for the grofwild reporting at three geographical levels

* flanders (gewesten): Vlaams Gewest
* provinces (provincies): West-Vlaanderen, Oost-Vlaanderen, Antwerpen, Vlaams Brabant, Limburg
* communes (gemeenten): 308 communes following "referentiebestand gemeentegrenzen 2016-01-29"
* faunabeheerzones: 11 meaningful regions defined by natural and unnatural borders
* fbz_gemeentes: overlap of faunabeheerzones and communes

Data projection is EPSG:31370. 
Source: [www.geopunt.be](http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen)
Conversion of data from source to geojson (in terminal)
`$ ogr2ogr -f "GeoJSON"  -t_srs "EPSG:31370" -s_srs "EPSG:31370" "flanders.geojson" "Refgew.shp"`


## R data files

The `spatialData.RData` contains a list with for each spatial level a SpatialPolygonsDataFrame object, with polygons and data as loaded with the R function `readShapeData()`. Spatial levels are flanders, provinces, communes, provincesVoeren (Voeren as separate province), FBZ (faunabeheerzones) and DFBZ (deel-faunabeheerzones). When any of the geosjon files are changed, this object should be updated by executing in R `readShapeData()`.
The object `spatialData` can easily be loaded in R by

`dataDir <- system.file("extdata", package = "reportingGrofwild")`

`load(file = file.path(dataDir, "spatialData.RData"))`


## reference data

* `Toekenningen_ree.csv`: this provides the *toegekende* numbers per province/year, will be updated each year.

| Kolom     | Eigenschappen | Toelichting                            |
| --------- | ------------- | -------------------------------------- |
| Provincie | str           | Provincie-namen                        |
| Jaar      | int           | het jaar van afschot (2002-..., of NA) |
| Labeltype | str           | het type ree                           |
| Aantal    | int           | toegekende aantal afschot              |


* `Openingstijden_grofwild.csv`:  De periode waarin het wildseizoen open is op jaarbasis voor elke wild soort/type:

| Kolom      | Eigenschappen     | Toelichting                          |
| ---------- | ----------------- | ------------------------------------ |
| Soort      | str               | het type wild                        |
| Type       | int               | het type wildsoort                   |
| Jaar       | str               | jaar van het overeenkomstige seizoen |
| Startdatum | date (dd/mm/yyyy) | startdatum voor dat jaar             |
| Stopdatum  | date (dd/mm/yyyy) | einddatum voor dat jaar              |


## GIS reference data

* `gemeentecodes.csv`

| Kolom    | Eigenschappen | Toelichting              |
| -------- | --------------| ------------------------ |
| NIS code | int           | de NIS code van gemeente |
| Postcode | int           | de postcode van gemeente |
| Gemeente | str           | de naam van gemeente     |


