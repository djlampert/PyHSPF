The examples provided in this folder illustrate how to use some of tools in PyHSPF for downloading, extracting, and processing geographic information systems (GIS) data. HSPF models require hydrography data such as average slope and elevation in a basin, elevations at the reach inlet and outlet, latitudes, longitudes, etc. HSPF models also often employ land use data to subdivide basins into hydrologically-similar units (PERLNDs). PyHSPF integrates built-in Python modules with extension modules (PyShp and GDAL) to extract data from a few particularly useful publically-available data sets including the National Hydrography Dataset Plus Version 2 (NHDPlus), the Cropland Data Layer (CDL), the National Inventory of Dams (NID), and the National Water Information System (NWIS). These tools can expedite extraction and integration of data in new watersheds into HSPF models in the United States. Similar tools could be developed for many other datasets such as hydrography in other countries or landuse datasets with different categories/emphasis. The scripts illustrate how to use PyHSPF to do the following:

- **nhdplusexample01.py:** download and extracts NHDPlus data
- **nhdplusexample02.py:** delineate the watershed for a point using NHDPlus data
- **cdlexample01.py:**     download and work with state-level land use data from the CDL
- **cdlexample02.py:**     download and work with bounding box data from the CDL
- **nidexample.py:**       download and work with NID data
- **ftable01.py:**         download and analyze NWIS gage data and stream measurements for development of FTABLES
- **ftable02.py:**         extend/interpolate FTABLES from gage stations upstream in a watershed
- **huc8example.py:**      divide a HUC8 into subbasins based on user-defined criteria

