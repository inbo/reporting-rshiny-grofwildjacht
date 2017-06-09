Column metadata of the `rshiny_reporting_data_*.csv`- files:

| Kolom | Eigenschappen | Toelichting | 
| ------|---------------|-------------|
| ID    | int           | random ID   |
| wildsoort |  factor | het type wildsoort |
|afschotjaar|int| het jaar van afschot (2002-2017, of NA)|
|afschot_datum |date |datum van afschot |
|postcode_afschot_locatie |int |de postcode van het afschot|
|gemeente_afschot_locatie |factor |de gemeente van het afschot|
|provincie |factor |de provincie van het afschot (Voeren afzonderlijk van Limburg behalve voor ree)|
|geslacht.MF |factor |het geslacht zoals weergegeven op het meldingsformulier|
|leeftijdscategorie_MF |factor |de leeftijdscategorie zoals weergegeven op het meldingsformulier|
|ontweid_gewicht |int |het ontweid of leeggewicht (gewicht zonder ingewanden)|
|aantal_embryos_onbekend |int |aanduiding waar of niet of het aantal embryo's gekend is of niet|
|aantal_embryos |int |het aantal embryo's|
|onderkaaklengte_links |int |de onderkaaklengte van de linker kaak gemeten door de jagers|
|onderkaaklengte_rechts |int |de onderkaaklengte van de rechter kaak gemeten door de jagers|
|doodsoorzaak |factor |de doodsoorzaak van het dier|
|leeftijd_maanden|int|de leeftijdsbepaling in maanden door het INBO|
|lengte_mm |int |de onderkaaklengte gemeten door het INBO |
|Leeftijdscategorie_onderkaak |factor |de leeftijdscategorie door het INBO|
