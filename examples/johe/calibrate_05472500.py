# calibrate_05472500.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 06/29/2015
# 
# Purpose: shows how to use the Preprocessor class to gather all the input
# data needed to create an HSPF model for an 8-digit hydrologic unit code 
# (HUC8), integrate the data to create an HSPFModel class for the HUC8,
# create a submodel for the gage for watershed associated with NWIS gage
# 05472500, calibrate the model for given parameters, and postprocess and 
# plot the results.

import os, pickle, datetime

# import the AutoCalibrator

from pyhspf.calibration import AutoCalibrator

# paths to the HSPF data files

destination = 'C:/HSPF_data'

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# NWIS gage for the calibration

gageid = '05472500'

# start and end dates for the calibration simulations (1981 to 2001)

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(1995, 1, 1)

# land use year to use for the model

landuseyear = 2001

# warmup time (model output for this number of days is ignored)

warmup = 366

# HSPF PERLND variables to use for calibration and inital values relative to 
# the PyHSPF defaults; these represent the "starting point" for the calibration
# variables that are then modified using the "path-of-steepest ascent" 
# approach to maximize the optimization parameter

variables = {'LZSN':   1.,
             'UZSN':   3.30,
             'LZETP':  1.5,   
             'INFILT': 0.19,
             'INTFW':  2.,
             'AGWRC':  1.02,
             'IRC':    0.82,
             'KVARY':  0.05,
             }

# optimization parameter 

optimization = 'Nash-Sutcliffe Product' 

# degrees of perturbation

perturbations = [2, 1, 0.5]
    
# parallel flag and number of processors to use (default uses them all)

parallel    = True
nprocessors = 3

# make sure the directory paths exist

if not os.path.isdir(destination):
    print('error, directory ' +
          '{} doesn not exist (please update)'.format(destination))
    raise

# Because parallel processing is (optionally) used, the process method has 
# to be called at runtime as shown below

if __name__ == '__main__': 

    # working directory for calibration simulations

    directory = '{}/{}/hspf'.format(destination, HUC8)

    # path to the baseline model

    filename = '{}/{}baseline'.format(directory, landuseyear)

    # file path to place the calibrated model and results

    calibration = '{}/{}/calibration'.format(destination, HUC8)

    # path where the calibrated model will be saved/located

    calibrated = '{}/{}'.format(calibration, gageid)

    # make the directory for the calibration simulations

    if not os.path.isdir(calibration): os.mkdir(calibration)

    # make an instance of the autocalibrator and give it the path to the model,
    # the start and end dates for the calibration period, the working directory 
    # location to use for simulation input and output files, the NWIS id 
    # (or comid) of the gage, and the HSPF modules to use (SNOW, PWATER, etc.)

    calibrator = AutoCalibrator(filename, start, end, directory, 
                                warmup = warmup, gageid = gageid, atemp = True,
                                snow = True, hydrology = True)

    # calibrate the model and save it to the "calibrated" location

    calibrator.autocalibrate(calibrated,
                             variables = variables, 
                             optimization = optimization,
                             perturbations = perturbations,
                             parallel = parallel,
                             nprocessors = nprocessors,
                             )
