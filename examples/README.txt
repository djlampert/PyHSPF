The examples provided in this folder illustrate how to use PyHSPF and the associated utilities. The examples are organized into several categories to illustrate different concepts needed to use HSPF and how PyHSPF can help accomplish these tasks. Here is a summary of the files and the concepts illustrated in the file. 

Introduction -- These examples are designed to illustrate how HSPF organizes information about the world into a computer program and how PyHSPF can be used to interact with HSPF. I would suggest going through these first to understand how HSPF works and how to use Python to supply the necessary information to HSPF. The other sections illustrate how to use some data tools to get the information into Python that is supplied directly in these scripts.

intro01.py -- Building an HSPF model (hypothetical example with no external data sources)
intro02.py -- Using python scripting to re-create the Hunting Creek Daily HSPF model 
intro03.py -- Building the Hunting Creek HSPF model with an hourly time step
intro04.py -- Utilizing the PyHSPF Postprocessor class to analyze the Hunting Creek example
intro05.py -- Performing a calibration of the Hunting Creek watershed with Python and PyHSPF

Tests -- These scripts were developed to run the test simulations distributed with the HSPF source code. They use data in the files located in the "data/calibrated" directory, and make output files there. These scripts may be of interest to people who are already familiar with HSPF. Information from the real world must be translated into the User Control Input (UCI) and Watershed Data Management (WDM) files to perform HSPF simulations. Existing UCI files could be adapted and run in Python using the approaches outlines in these examples. The scripts do the following:

test01.py -- shows how to create the "test.wdm" file, runs "test01.uci" and plots the data
test02.py -- adds datasets to "test.wdm" and runs "test02.uci"
test03.py -- adds datasets to "test.wdm" and runs "test03.uci"
tests04_15.py -- runs test04.uci through test15.uci simulations (no WDM file manipulation)
tests04_15_parallel.py -- shows how to run HSPF simulations in parallel

Special Actions -- This section needs work, but there is one example. Special Actions are designed to represent how the activities of humans modify hydrological processes in HSPF. Some examples of this include tilling, harvesting, pesticide and fertilizer applications.

specialactions01.py -- similar to intro01.py but illustrates the use of "Special Actions"

GIS data examples -- Python has many modules that are useful for working with Geographic Information Systems (GIS) data. HSPF models require hydrography data such as average slope within a basin, average elevation in a basin, elevations at the reach inlet and outlet, latitudes, longitudes, etc. HSPF modeling typically employs land use data to subdivide basins into hydrologically-similar units (PERLNDs). 

PyHSPF integrates built-in Python modules with extension modules (PyShp and GDAL) to extract data from a few particularly useful publically-available data sets including the National Hydrography Dataset Plus Version 2 (NHDPlus), the Cropland Data Layer (CDL), and the National Inventory of Dams (NID). These tools can expedite extraction and integration of data in new watersheds into HSPF models in the United States. It's worth noting that similar tools could be developed for many other datasets such as hydrography in other countries or landuse datasets with different categories/emphasis. The following scripts illustrate how to use PyHSPF GIS tools:

nhdplusexample01.py -- uses the NHDPlusExtractor to download and extract NHDPlus data
nhdplusexample02.py -- uses the NHDPlusDelineator to delineate the watershed for a point
cdlexample.py       -- uses the CDLExtractor to work with landuse data from the CDL
nidexample.py       -- uses the NIDExtractor to download and work with NID data
huc8example.py      -- divides a HUC8 into subbasins with the HUC8Delineator and other tools

Time series data extraction -- HSPF is centered around manipulations of time series, and thus it is necessary to supply many external time series to use HSPF. At a minimum, at least one time series is needed for precipitation, potential evapotranspiration, and stream flow. To use the snow melt modules requires the addition of a minimum one more time series for temperature. Given that potential evapotranspiration and snow melt depend on solar radiation, wind, and humidity or dew point, these time series are also needed to use HSPF. Observed pan evaporation can be compared with estimated potential evapotranspiration time series used in the development of HSPF models. Snowfall and snowdepth are predicted from the other timeseries in HSPF, so it is useful to compare snow observations with HSPF predictions.

PyHSPF has utilities to gather all these time series data together from several publically-available databases. The National Water Information System (NWIS) contains flow and water quality time series data. The Global Historical Climate Network Daily (GHCND) contains many climate time series including min and max temperature, snowfall, snowdepth, wind speed, and pan evaporation. The Global Summary of the Day (GSOD) data contains min and max temperature, dew point, and wind speed. The National Solar Radiation Database (NSRDB) has hourly estimates of solar radiation based on several models in addition to a limited amount of observed solar radiation data. The hourly precipitation database (3240) from the National Climate Data Center contains data from stations with hourly observations. Given the extreme spatial and temporal variability and the obvious importance of precipitation for hydrologic modeling, this is a particularly important dataset. The following scripts show how to use PyHSPF tools to gather relevant time series for HSPF:
 
nwisexample.py   -- uses the NWISExtractor to extract NWIS flow and water quality data
ghcndexample.py  -- uses climateutils to extract GHCND data
precipexample.py -- uses climateutils to extract hourly precipitation data
gsodexample.py   -- uses climateutils to extract GSOD data
nsrdbexample.py  -- uses climateutils to extract NSRDB data

Climate Time Series Processing -- The raw data gathered from the various databases is typically not suitable for hydrologic modeling for a variety of reasons. For example, some of the data may be missing or errors may have been made in reporting. HSPF models require estimates of the conditions throughout the watershed, so "point" observations must be interpolated/extrapolated throughout the watershed. There are an infinite number of different methods for aggregating and disaggregating climate data series. PyHSPF has a ClimateProcessor class to assist with these tasks. The following scripts illustrate the climate processing tools available.

climateprocessor01.py -- downloads all climate data for a region into an organized structure
climateprocessor02.py -- aggregates GHCND temperature data into a single timeseries

The "hunting.py" example illustrates the many features of PyHSPF; the script downloads 
source data from NHDPlus, extracts data for a HUC8, extracts and delineates the 
hydrography data for the watershed of the gage in the HUC8, builds an instance of the 
Watershed class from the data, builds the HSPFModel, and uses the autocalibration
routines to calibrate the hydrology process parameters including the initial values.

The "hbnexample.py" script reads data from an HSPF binary output file using the PyHSPF
HBNReader class.

The "hspexp.py" script runs the files in the data/calibrated directory. These files were
calibrated with HSPEXP. The script then analyzes and plots the results.

Suggestions and questions are always welcome. Note that these examples may not be compatible with older versions of the software.
