# qcew-employment-county-to-congress-districts
This code aggregates annual US **county level employment** data from the Quarterly Census of Employment and Wages (QCEW) by industry (NAICS) to the **Congressional district level** for the 108th to 115th Congress (2003 - 2018) and adjusts for redistricting where Congress district boundaries have been changed outside the usual decennial rhythm.

## Code:
The `QCEW_employment_congress_district_level.R` code loads the QCEW county level employment data depending on the **NAICS industries** and **years** specified by the user. It then aggregates this data from the county to the Congress district level while accounting for redistricting. (This code relies on the data source 1-3.)

The `QCEW_employment_state_level.R` code loads the QCEW state level employment data depending on the **NAICS industries** and **years** specified by the user. (This code only relies on the data source 1.)

## Data:

Three data sources are used:

1. `QCEW county level employment data` (CSVs By Industry - Annual Averages). The data can be downloaded here: https://www.bls.gov/cew/downloadable-data-files.htm)

    Source: U.S. Department of Labor. Bureau of Labor Statistics (2021) _Quarterly Census of Employment and Wages_.

2. `Census 2000 county to Congress district aggregation factors` provided by the Geographic Correspondence Engine (Geocorr 2000). Data required for this script provided above. The data can otherwise be regenerated here: https://mcdc.missouri.edu/applications/geocorr2000.html

    Source: Missouri Census Data Center (2010) _Geocorr 2000: Geographic Correspondence Engine_ (Version 1.3.3 with Census 2000 Geography).

3. `Redistricting aggregation factors` provided by Autor, Dorn, Hanson and Majlesi. The Political Geography Crosswalk files G1, G2, G3 & G4 can be downloaded here: https://www.ddorn.net/data.htm

    Source: David Autor, David Dorn, Gordon Hanson and Kaveh Majlesi (2020) _Importing Political Polarization? The Electoral Consequences of Rising Trade Exposure._ American Economic Review, 110(10): 3139-3189.

Note: All files should be saved in one parent folder called `Data`, containing a folder called `QCEW` for the QCEW county employment data (1.), a folder called `geocorr2000` for the county to Congress district aggregation data (2.) and a folder called `PoliticalGeography` for redistricting crosswalk files (3.).
