
In this folder, the required data is collected to create the Rshiny application.

## reporting data files

### grofwild 

The `rshiny_reporting_data_*.csv` are providing the raw data and numbers of the grofwild reporting, which will be updated when new data is available. 
In general "_comp" variables are computed as follows: if info from INBO is known then this is used, otherwise info from MF (meldingsformulier) is used. If both are missing, then value NA is assigned.

Column metadata of the `rshiny_reporting_data_*.csv`- files is as follows:

| Kolom                        | Eigenschappen | Toelichting                              |
| ---------------------------- | ------------- | ---------------------------------------- |
| ID                           | int           | uniek ID, koppelt ecology en geography data    |
| wildsoort                    | factor        | het type wildsoort                       |
| afschotjaar                  | int           | het jaar van afschot (2002-2020, of NA)  |
| afschot_datum                | date          | datum van afschot  (YYYY-MM-DD)          |
| doodsoorzaak                 | factor        | de doodsoorzaak van het dier (afschot)   |
|                              |               |                                          |
| postcode_afschot_locatie     | int           | de postcode van het afschot              |
| gemeente_afschot_locatie     | factor        | de gemeente van het afschot              |
| provincie                    | factor        | de provincie van het afschot (Voeren afzonderlijk van Limburg behalve voor ree) |
| FaunabeheerZone              | int           | de faunabeheerzone van het afschot (1 tem 10)    |
| FaunabeheerDeelzone          | factor        | faunabeheerdeelzone - subset van Faunabeheerzone |
| UTM5                         | factor        | UTM5 index                               |
|                              |               |                                          |
| geslacht_comp                  | factor        | compilatie voor geslacht: INBO indien gekend, anders MF |
| geslacht_comp_bron           | factor        | welke bron wordt gebruikt voor geslacht_comp                           |
|                              |               |                                          |
| leeftijdscategorie_MF        | factor        | de leeftijdscategorie zoals weergegeven op het meldingsformulier |
| Leeftijdscategorie_onderkaak | factor        | de leeftijdscategorie door het INBO      |
| leeftijd_comp                | factor        | compilatie voor de leeftijdscategorie: INBO indien gekend, anders MF |
| leeftijd_comp_bron           | factor        | welke bron wordt gebruikt voor leeftijd_comp (inbo of meldingsformulier |
| leeftijd_maanden             | int           | de leeftijdsbepaling in maanden door het INBO |
|                              |               |                                          |
| ontweid_gewicht              | int           | het ontweid of leeggewicht (gewicht zonder ingewanden) |
|                              |               |                                          |
| aantal_embryos_onbekend      | boolean       | aanduiding TRUE/FALSE of het aantal embryo's gekend |
| aantal_embryos               | int           | het aantal embryo's                      |
| aantal_embryos_bron          | factor        | welke bron wordt gebruikt voor aantal_embryo's (inbo of meldingsformulier) |
|                              |               |                                          |
| onderkaaklengte_links        | int           | de onderkaaklengte van de linker kaak volgens MF |
| onderkaaklengte_rechts       | int           | de onderkaaklengte van de rechter kaak volgens MF |
| lengte_mm                    | int           | de onderkaaklengte gemeten door het INBO |
| onderkaaklengte_comp         | int           | compilatie van de onderkaaklengte: INBO indien gekend, anders MF  |
| onderkaaklengte_comp_bron    | factor        | welke bron wordt gebruikt voor onderkaaklengte_comp (inbo of meldingsformulier) |
|                              |               |                                          |
| type_comp                    | factor        | combinatie van leeftijd_comp, geslacht_comp en wildsoort |


### wildschade

The `Wildschade_georef.csv` file is providing the raw data and numbers of the wildschade reporting, which will be updated when new data is available.

Column metadata of the `Wildschade_georef.csv`- file after filtering necessary variables is as follows:
In the R-code we rename column variables as indicated in the "Nieuwe Naam" column.

| Kolom                        | Nieuwe Naam     | Eigenschappen | Toelichting                             |
| ---------------------------- | --------------- | ------------- |---------------------------------------- |
| UUID                         | UUID            | int           | uniek ID                                 |
| IndieningID                  | caseID          | int           | case ID - soms meerdere UUID per case    |
| Jaartal                      | afschotjaar     | int           | het jaar van het schadegeval (2006-2020) |
| IndieningSchadeBasisCode     | schadeBasisCode | factor        | verzamel code van het type schadegeval (ANDERE, GEWAS, VRTG)  |
| IndieningSchadeCode          | schadeCode      | factor        | detail code van het type schadegeval (subset van schadeBasisCode) |
| SoortNaam                    | SoortNaam       | factor        | het type gewas dat werd beschadigd (enkel voor GEWAS schade)  |
| DierSoortNaam                | wildsoort       | factor        | het type dier                            |
| DatumVeroorzaakt             | afschot_datum   | date          | datum van schadegeval  (DD/MM/YYYY)      |
| provincie                    | provincie       | factor        | de provincie van het schadegeval         |
| fbz                          | FaunabeheerZone | int           | de faunabeheerzone van het schadegeval (1 tem 10 or NA)  |
| fbdz                         | fbdz            | factor        | faunabeheerzone van schadegeval (subset van fbz)    |
| NisCode_Georef               | NISCODE         | factor        | de niscode van het schadegeval           | 
| GemNaam_Georef               | gemeente_afschot_locatie     | factor        | de gemeente van het schadegeval          |
| UTM5                         | UTM5code        | factor        | de 5 x 5 UTM code van het schadegeval    |
| PolyLocatieWKT               | perceelPolygon  | factor        | polygon coordinaten van het schadegeval  |
| x                            | x               | numeric        | x-coordinaat van schadegeval locatie     |
| y                            | y               | numeric        | y-coordinaat van schadegeval locatie     |



## GIS data files

The `*.geojson` files are providing GIS data for the grofwild reporting at the geographical levels

* flanders (gewesten): Vlaams Gewest
* provinces (provincies): West-Vlaanderen, Oost-Vlaanderen, Antwerpen, Vlaams Brabant, Limburg
* communes (gemeenten): 308 communes following "referentiebestand gemeentegrenzen 2016-01-29"
* faunabeheerzones: 11 meaningful regions defined by natural and unnatural borders
* fbz_gemeentes: overlap of faunabeheerzones and communes
* utm5: 5 x 5 UTM squares

Data projection is EPSG:31370. 
Source: [www.geopunt.be](http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen)
Conversion of data from source to geojson (in terminal)
`$ ogr2ogr -f "GeoJSON"  -t_srs "EPSG:31370" -s_srs "EPSG:31370" "flanders.geojson" "Refgew.shp"`


## R data files

The `spatialData.RData` contains a list with for each spatial level a SpatialPolygonsDataFrame object, with polygons and data as loaded with the R function `createShapeData()`. Spatial levels are flanders, provinces, communes, provincesVoeren (Voeren as separate province), faunabeheerzones, fbz_gemeentes and utm5. When any of the geosjon files are changed, this object should be updated by executing in R `readShapeData()`. Next, install the package such that the latest shape data are available in the extdata folder.
The object `spatialData` can easily be loaded in R by

`dataDir <- system.file("extdata", package = "reportingGrofwild")`

`load(file = file.path(dataDir, "spatialData.RData"))`


## reference data

* `Toekenningen_ree.csv`: this provides the *toegekende* numbers per province/year, will be updated each year.

| Kolom     | Eigenschappen | Toelichting                            |
| --------- | ------------- | -------------------------------------- |
| Provincie | factor        | Provincie-namen                        |
| Jaar      | int           | het jaar van afschot (2002-..., of NA) |
| Labeltype | str           | het type ree                           |
| Aantal    | int           | toegekende aantal afschot              |


* `Openingstijden_grofwild.csv`:  De periode waarin het wildseizoen open is op jaarbasis voor elke wild soort/type:

| Kolom      | Eigenschappen     | Toelichting                          |
| ---------- | ----------------- | ------------------------------------ |
| Soort      | str               | het type wild                        |
| Type       | str               | het type wildsoort                   |
| Jaar       | int               | jaar van het overeenkomstige seizoen |
| Startdatum | date (dd/mm/yyyy) | startdatum voor dat jaar             |
| Stopdatum  | date (dd/mm/yyyy) | einddatum voor dat jaar              |


## GIS reference data

* `gemeentecodes.csv`: Used to match NIS.code to postcode and name for each gemeente (commune) in Flanders.

| Kolom    | Eigenschappen | Toelichting              |
| -------- | --------------| ------------------------ |
| NIS.code | int           | de NIS code van gemeente |
| Postcode | int           | de postcode van gemeente |
| Gemeente | str           | de naam van gemeente     |


