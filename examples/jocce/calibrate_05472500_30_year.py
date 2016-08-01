# calibration_05472500_30_year.py
#
# David J. Lampert
#
# 06/28/2016
#
# shows how to use PyHSPF to gather input data, build a baseline model,
# and calibrate the hydrological process parameters over a 30-year period.
#
# import Python built-in modules

import os, pickle, datetime

# import PyHSPF classes

from pyhspf               import Postprocessor
from pyhspf.preprocessing import Preprocessor
from pyhspf.calibration   import AutoCalibrator

# paths to the HSPF/watershed data files
# network is the path to place/read the (large) NHDPlus, NID, and NWIS raw data
# destination is the path to place HUC8-specific files/models

network     = 'D:'
destination = 'C:/HSPF_data'

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# NWIS gage id for the calibration

gageid = '05472500'

# start and end dates for the calibration simulations (1981 to 2001)

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# maximum drainage area for subbasins in square kilometers

drainmax = 400

# comma separated value file linking land use codes from the Cropland Data
# Layer to aggregated land use categories for HSPF land segments

aggregation = 'data/cdlaggregation.csv'

# comma separated value file of parameters for the HSPF land use categories
# including RGB values for plots and evapotranspiration crop coefficients

landuse = 'data/lucs.csv'

# land use year to use for the model

landuseyear = 2001

# warmup time (model output for this number of days is ignored)

warmup = 366

# HSPF PERLND variables to use for calibration and inital values relative to 
# the PyHSPF defaults; these represent the "starting point" for the calibration
# variables that are then modified using the "path-of-steepest ascent" 
# approach to maximize the optimization parameter

variables = {'LZSN':   1.,
             'UZSN':   1,
             'LZETP':  1,   
             'INFILT': 1,
             'INTFW':  1.,
             'AGWRC':  1.,
             'IRC':    1,
             'KVARY':  0.02,
             }

# calibrated values (the optimization completes more quickly starting here)

#variables = {'LZSN':   1.08,
#             'UZSN':   1.16,
#             'LZETP':  1.39,   
#             'INFILT': 0.75,
#             'INTFW':  1.16,
#             'AGWRC':  0.99,
#             'IRC':    0.79,
#             'KVARY':  0.033,
#             }

# optimization parameter 

optimization = 'Nash-Sutcliffe Efficiency'

# degrees of perturbation

perturbations = [2, 1, 0.5]
    
# parallel flag and number of processors to use (default uses them all)

parallel    = True
nprocessors = 5

# Because parallel processing is (optionally) used, the process method has 
# to be called at runtime as shown below (nothing below this point should
# be modified)

if __name__ == '__main__': 

    # check to make sure the directory and file paths exist and warn if not

    if not os.path.isdir(network):
        print('error, directory ' +
              '{} does not exist (please update)'.format(network))
        raise
    if not os.path.isdir(destination):
        print('error, directory ' +
              '{} does not exist (please update)'.format(destination))
        raise
    if not os.path.isfile(aggregation):
        print('error, file ' +
              '{} does not exist (please update)'.format(aggregation))
        raise
    if not os.path.isfile(landuse):
        print('error, file {} does not exist (please update)'.format(landuse))
        raise

    # working directory for calibration simulations

    directory = '{}/{}/hspf'.format(destination, HUC8)

    # make an instance of the Preprocessor

    preprocessor = Preprocessor()

    # set up the directory locations

    preprocessor.set_network(network)
    preprocessor.set_output(destination)

    # set the simulation-specific parameters

    preprocessor.set_parameters(HUC8 = HUC8,
                                start = start,
                                end = end,
                                cdlaggregate = aggregation,
                                landuse = landuse)

    # preprocess the HUC8

    preprocessor.preprocess(drainmax = drainmax)

    # build the HSPFModel and turn on the flags for the air temperature 
    # (ATEMP for PERLNDs and IMPLNDs), snow (SNOW for PERLNDs and IMPLNDs), 
    # and hydrology (PWATER for PERLNDs, IWATER for IMPLNDs, HYDR for RCHRESs)
    # for the resulting model (the flags for all modules default to "False",
    # so no simulation options will be used unless they are activated)

    preprocessor.build_hspfmodel(landuseyear = landuseyear, atemp = True,
                                 snow = True, hydrology = True)

    # the routine in the previous step will build and pickle an HSPFModel
    # to the destination/hspf directory named "<landuseyear>baseline" (in this
    # case 2001) stored in the "hspfmodel" attribute of the preprocessor

    filename = '{}/{}baseline'.format(directory, landuseyear)

    # make a directory for the preliminary calibration

    preliminary = '{}/{}/preliminary'.format(destination, HUC8)

    # make the directory for the calibration simulations

    if not os.path.isdir(preliminary): os.mkdir(preliminary)

    print('saving the preliminary (uncalibrated) results\n')

    with open(filename, 'rb') as f: hspfmodel = pickle.load(f)

    # get the comid of the gage

    d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
    comid = d[gageid]

    # change the filename

    hspfmodel.filename = '{}/{}'.format(preliminary, gageid)

    # build the input WDM file

    hspfmodel.build_wdminfile()

    # output variables

    targets = ['water_state', 
               'reach_outvolume', 
               'evaporation', 
               'runoff', 
               'groundwater',
               'snow_state', 
               'snowpack', 
               'snowfall',
                ]

    # build the UCI and output WDM files

    hspfmodel.build_uci(targets, start, end, atemp = True, snow = True,
                        hydrology = True)

    # run it

    hspfmodel.run(verbose = True)

    # use the Postprocessor to analyze and save the results

    dates = start + datetime.timedelta(days = warmup), end

    postprocessor = Postprocessor(hspfmodel, dates, comid = comid)

    postprocessor.get_hspexp_parameters(verbose = False)
    postprocessor.plot_hydrograph(tstep = 'monthly', show = False,
                                  output = '{}/hydrography'.format(preliminary))
    postprocessor.plot_calibration(output = '{}/statistics'.format(preliminary),
                                   show = False)
    output = '{}/calibration_report.csv'.format(preliminary)
    postprocessor.calibration_report(output = output)
    postprocessor.plot_snow(output = '{}/snow'.format(preliminary), 
                            show = False)
    postprocessor.plot_dayofyear(output = '{}/dayofyear'.format(preliminary),
                                 show = False)

    # have to close the WDM files to continue
    
    postprocessor.close()

    # calibration using the full period of record

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

    if not os.path.isfile(calibrated):
        
        calibrator = AutoCalibrator(filename, start, end, directory, 
                                    warmup = warmup, gageid = gageid,
                                    atemp = True, snow = True, hydrology = True)

        # calibrate the model and save it to the "calibrated" location

        calibrator.autocalibrate(calibrated,
                                 variables = variables, 
                                 optimization = optimization,
                                 perturbations = perturbations,
                                 parallel = parallel,
                                 nprocessors = nprocessors)

    # run a simulation to analyze calibration goodness of fit
        
    with open(calibrated, 'rb') as f: hspfmodel = pickle.load(f)

    # find the comid of the gage

    d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
    comid = d[gageid]

    # build the input WDM file

    hspfmodel.build_wdminfile()

    # build the UCI and output WDM files

    hspfmodel.build_uci(targets, start, end, atemp = True, snow = True,
                        hydrology = True)

    # run it

    hspfmodel.run(verbose = True)

    # use the Postprocessor to analyze and save the results

    dates = start + datetime.timedelta(days = warmup), end

    postprocessor = Postprocessor(hspfmodel, dates, comid = comid)

    postprocessor.get_hspexp_parameters(verbose = False)
    postprocessor.plot_hydrograph(tstep = 'monthly', show = False,
                                  output = '{}/hydrography'.format(calibration))
    postprocessor.plot_calibration(output = '{}/statistics'.format(calibration),
                                   show = False)
    postprocessor.plot_runoff(tstep = 'daily', show = False,
                              output = '{}/runoff'.format(calibration))
    output = '{}/calibration_report.csv'.format(calibration)
    postprocessor.calibration_report(output = output)
    postprocessor.plot_snow(output = '{}/snow'.format(calibration), 
                            show = False)
    postprocessor.plot_dayofyear(output = '{}/dayofyear'.format(calibration),
                                 show = False)
    postprocessor.plot_storms(season = 'all', show = False, 
                              output = '{}/storms'.format(calibration))
