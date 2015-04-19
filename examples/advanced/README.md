The examples provided in this folder illustrate how to use PyHSPF to do some more complicated tasks. It would be great to add any interesting new applications to this directory.

The "hunting.py" example illustrates the many features of PyHSPF; the script downloads source data from NHDPlus, extracts data for a HUC8, extracts and delineates the hydrography data for the watershed of the gage in the HUC8, builds an instance of the Watershed class from the data, builds the HSPFModel, and uses the autocalibration routines to calibrate the hydrology process parameters including the initial values.

The "hspexp.py" script runs the files in the data/calibrated directory. These files were calibrated with HSPEXP. The script then analyzes and plots the results.

