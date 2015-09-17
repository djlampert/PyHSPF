The examples scripts in this directory illustrate how to use PyHSPF to do some more complicated tasks. It would be great to add new applications.

- **hunting.py:** This example downloads source data from NHDPlus, extracts and delineates the hydrography data for the watershed of the gage in the HUC8, builds an instance of the Watershed class from the data, builds the HSPFModel, and uses the autocalibration routines to calibrate the hydrology process parameters including the initial values.
- **hspexp.py:** This example runs the model in the data/calibrated directory. These files were calibrated with HSPEXP. The script then analyzes and plots the results.
- **preprocess_07080106.py:** This example shows how to use the Preprocessor to download and extract all data needed for an HSPF model for the North Skunk River Watershed, IA. The script describes the directories and file structure created by the Preprocessor in detail, and includes data for construction of a baseline HSPFModel.
- **preprocess_02040101.py:** Another preprocessing example for the Delaware River Basin, which is in two different states.
- **preprocess_07100008.py:** Another preprocessing example for the Lake Red Rock Watershed in Iowa.
- **preprocess_05472500.py:** Similar to the preprocess_07080106.py, but less detail and includes the construction and simulation of the baseline model (before calibration).
- **calibrate_05472500.py:** Illustrates how to use the AutoCalibrator class to automatically calibrate the HSPFModel constructed in the preprocess_05472500.py example.
- **postprocess_05472500.py:** Runs the calibrated model and generates results.
- **validate_05472500.py:** Runs the calibrated model over a different period of time and generates validation results.
- **create_single_gage.py:** Uses the model constructed in preprocess_05472500.py and data from two BASINS WDM files to build a model with a single timeseries.