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

# Paths to working directories for source NHDPlus, CDL, NWIS, NID datasets
# (modify as needed for the PC of interest)

network     = 'Z:'
destination = 'C:/HSPF_data'

if not os.path.isdir(network):
    print('error, directory {} doesn not exist (please update)'.format(network))
    raise
if not os.path.isdir(destination):
    print('error, directory ' +
          '{} doesn not exist (please update)'.format(destination))
    raise

# import the Preprocessor, Postprocessor, and AutoCalibrator

from pyhspf.preprocessing import Preprocessor
from pyhspf               import Postprocessor
from pyhspf.calibration   import AutoCalibrator

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# two-digit state abbreviation for the CDL

state = 'Iowa'

# NWIS gage for the calibration

gageid = '05472500'

# start and end dates for the model (1981 to 2009)

start = datetime.datetime(1981, 1, 1)
end   = datetime.datetime(2009, 1, 1)

# maximum drainage area for subbasins in square kilometers

drainmax = 400

# comma separated value file linking land use codes from the Cropland Data
# Layer to aggregated land use categories for HSPF land segments

aggregation = 'cdlaggregation.csv'

# comma separated value file of parameters for the HSPF land use categories
# including RGB values for plots and evapotranspiration crop coefficients

landuse = 'lucs.csv'

# land use year to use for the model

landuseyear = 2001

# HSPF PERLND variables to use for calibration and inital values relative to 
# the PyHSPF defaults

variables = {'LZSN':   0.9,
             'UZSN':   4.7,
             'LZETP':  1.40,   
             'INFILT': 0.84,
             'INTFW':  1.52,
             'AGWRC':  1.02,
             'IRC':    1.05,
             'DEEPFR': 0.54,
             }

# optimization parameter 

optimization = 'Nash-Sutcliffe Efficiency' 

# degrees of perturbation

#perturbations = [2, 1, 0.5]
perturbations = [2]
    
# parallel flag and number of processors to use (default uses them all)

parallel    = True
nprocessors = 3

# working directory for calibration simulations

directory = '{}/{}/hspf'.format(destination, HUC8)

# file path to place the calibrated model and results

calibration = '{}/{}/calibrations'.format(destination, HUC8)

# path where the calibrated model will be saved/located

calibrated = '{}/{}'.format(calibration, gageid)

# Because parallel processing is (optionally) used, the process method has 
# to be called at runtime as shown below

if __name__ == '__main__': 

    # make an instance of the Preprocessor

    processor = Preprocessor()

    # set up the directory locations

    processor.set_network(network)
    processor.set_output(destination)

    # set the simulation-specific parameters

    processor.set_parameters(HUC8 = HUC8,
                             start = start,
                             end = end,
                             state = state,
                             cdlaggregate = aggregation,
                             landuse = landuse)

    # preprocess the HUC8

    processor.preprocess(drainmax = drainmax)

    # build the HSPFModel and turn on the flags for the air temperature 
    # (ATEMP for PERLNDs and IMPLNDs), snow (SNOW for PERLNDs and IMPLNDs), 
    # and hydrology (PWATER for PERLNDs, IWATER for IMPLNDs, HYDR for RCHRESs)
    # for the resulting model (the flags for all modules default to "False",
    # so no simulation options will be used unless they are activated)

    processor.build_hspfmodel(landuseyear = landuseyear, atemp = True,
                              snow = True, hydrology = True)

    # the routine in the previous step will build and pickle an HSPFModel
    # to the destination/hspf directory named "<landuseyear>baseline" (in this
    # case 2001) stored in the "hspfmodel" attribute of the preprocessor

    filename = '{}/{}baseline'.format(directory, landuseyear)

    # make the directory for the calibration simulations

    if not os.path.isdir(calibration): os.mkdir(calibration)

    # make an instance of the autocalibrator and give it the path to the model,
    # the start and end dates for the calibration period, the working directory 
    # location to use for simulation input and output files, the NWIS id 
    # (or comid) of the gage, and the HSPF modules to use (SNOW, PWATER, etc.)

    calibrator = AutoCalibrator(filename, start, end, directory, 
                                gageid = gageid, atemp = True, snow = True,
                                hydrology = True)

    # calibrate the model and save it to the "calibrated" location

    calibrator.autocalibrate(calibrated,
                             variables = variables, 
                             optimization = optimization,
                             perturbations = perturbations,
                             parallel = parallel,
                             nprocessors = nprocessors,
                             )

    print('\nsaving the calibration results\n')

    with open(calibrated, 'rb') as f: hspfmodel = pickle.load(f)

    # build the input WDM file

    hspfmodel.build_wdminfile()

    # output variables

    targets = ['water_state', 
               'reach_outvolume', 
               'evaporation', 
               'runoff', 
               'groundwater',
                ]

    # build the UCI and output WDM files

    hspfmodel.build_uci(targets, start, end, atemp = True, snow = True,
                        hydrology = True)

    # run it

    hspfmodel.run(verbose = True)

    # use the Postprocessor to analyze and save the results

    postprocessor = Postprocessor(hspfmodel, (start, end), 
                                  comid = calibrator.comid)

    postprocessor.get_hspexp_parameters()
    postprocessor.plot_hydrograph(tstep = 'monthly', 
                                  output = '{}/hydrography'.format(calibration))
    postprocessor.plot_calibration(output = '{}/statistics'.format(calibration))
    postprocessor.plot_runoff(tstep = 'daily', 
                              output = '{}/runoff'.format(calibration))

# Using the preprocessor in other watersheds/gages *should* be as simple as
# supplying the parameters above (start and end date, state, 8-digit HUC, 
# NWIS gage ID, land use year, maximum drainage area); if you try and 
# get an error please report it!
