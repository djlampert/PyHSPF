The examples scripts in this directory illustrate how to use PyHSPF to do some more complicated tasks. It would be great to add new applications.

- **hunting.py:** This example downloads source data from NHDPlus, extracts and delineates the hydrography data for the watershed of the gage in the HUC8, builds an instance of the Watershed class from the data, builds the HSPFModel, and uses the autocalibration routines to calibrate the hydrology process parameters including the initial values.
- **hspexp.py:** This example runs the model in the data/calibrated directory. These files were calibrated with HSPEXP. The script then analyzes and plots the results.
- **preprocess_07080106.py:** This exmaple shows how to use the Preprocessor to download and extract all data needed for an HSPF model for the North Skunk River Watershed, IA. The script describes the directories and file structure created by the Preprocessor in detail, and includes data for construction of a baseline HSPFModel.
- **preprocess_05472500.py:** Similar to the previous example, but less detail and includes the construction and simulation of the baseline model (before calibration).
- **calibrate_05472500.py:** Illustrates the use of the AutoCalibrator class, that automatically calibrates an existing HSPFModel.
- **postprocess_05472500.py:** Runs the calibrated model and generates results.
- **preprocess_02040101.py:** Another preprocessing example for the Delaware River Basin, which is in two different states.