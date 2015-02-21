The examples provided in this folder illustrate how to use some of tools in PyHSPF for downloading, extracting, and processing geographic information systems (GIS) data. HSPF models require hydrography data such as average slope within a basin, average elevation in a basin, elevations at the reach inlet and outlet, latitudes, longitudes, etc. HSPF models also often employ land use data to subdivide basins into hydrologically-similar units (PERLNDs). PyHSPF integrates built-in Python modules with extension modules (PyShp and GDAL) to extract data from a few particularly useful publically-available data sets including the National Hydrography Dataset Plus Version 2 (NHDPlus), the Cropland Data Layer (CDL), and the National Inventory of Dams (NID). These tools can expedite extraction and integration of data in new watersheds into HSPF models in the United States. It's worth noting that similar tools could be developed for many other datasets such as hydrography in other countries or landuse datasets with different categories/emphasis. The following scripts illustrate how to use PyHSPF GIS tools:

nhdplusexample01.py -- uses the NHDPlusExtractor to download and extract NHDPlus data
nhdplusexample02.py -- uses the NHDPlusDelineator to delineate the watershed for a point
cdlexample.py       -- uses the CDLExtractor to work with landuse data from the CDL
nidexample.py       -- uses the NIDExtractor to download and work with NID data
huc8example.py      -- divides a HUC8 into subbasins with the HUC8Delineator and other tools

