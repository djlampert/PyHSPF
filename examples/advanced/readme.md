The examples provided in this folder illustrate how to use PyHSPF to do some more complicated tasks. It would be great to add any interesting new applications to this directory.

**hunting.py** This example downloads source data from NHDPlus, extracts and delineates the hydrography data for the watershed of the gage in the HUC8, builds an instance of the Watershed class from the data, builds the HSPFModel, and uses the autocalibration routines to calibrate the hydrology process parameters including the initial values.

**hspexp.py** This example runs the files in the data/calibrated directory. These files were calibrated with HSPEXP. The script then analyzes and plots the results.

**preprocess_07080106.py** This exmaple shows how to use the preprocessor to download and extract all data needed for an HSPF model for the North Skunk River Watershed, IA.
 