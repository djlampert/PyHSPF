The examples provided illustrate how to use PyHSPF and how to think in HSPF. Here is a 
summary of the examples:

01. Building an HSPF model (hypothetical example with no external data sources)
02. Building the Hunting Creek HSPF model (the example from HSPExp) with a daily time step
03. Building the Hunting Creek HSPF model with an hourly time step
04. Utilizing the PyHSPF Postprocessor class to analyze the Hunting Creek example
05. Performing a calibration of the Hunting Creek watershed with Python and PyHSPF
06. Repeat of example01 but illustrating the use of Special Actions
07. Utilizing PyHSPF's extractor for NHDPlus data
08. Utilizing PyHSPF's extractor for NWIS data
09. Utilizing PyHSPF's NHDPlusDelineator for the watershed of a gage within a HUC8
10. Utilizing PyHSPF's CDLExtractor to download and extract data from the Cropland Data Layer
11. Utilizing PyHSPF's extractor for Global Historical Climate Network Daily data
12. Utilizing PyHSPF's extractor for all HSPF climate data needs

The "hunting.py" example illustrates the many features of PyHSPF; the script downloads 
source data from NHDPlus, extracts data for a HUC8, extracts and delineates the 
hydrography data for the watershed of the gage in the HUC8, builds an instance of the 
Watershed class from the data, builds the HSPFModel, and uses the autocalibration
routines to calibrate the hydrology process parameters including the initial values.

The "hspexp.py" script runs the files in the data/calibrated directory. These files were
calibrated with HSPEXP. The script then analyzes and plots the results.

Several other scripts have been developed to run the test simulations distributed with HSPF.
These scripts use data in the "data/calibrated" directory, and make output files there. The
scripts do the following:

test01.py -- shows how to create the "test.wdm" file, runs "test01.uci" and plots the data
test02.py -- adds datasets to "test.wdm" and runs "test02.uci"
test03.py -- adds datasets to "test.wdm" and runs "test03.uci"
tests04_15.py -- runs test04.uci through test15.uci simulations (no WDM file manipulation)
tests04_15_parallel.py -- shows how to run HSPF simulations in parallel

PyHSPF has functionalities beyond those described in these examples. Direct any
questions about functionality and applications to the developer. Note that these
examples may not be compatible with older versions of the software.
