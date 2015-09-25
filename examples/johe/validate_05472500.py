# calibrate_05472500.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 07/21/2015
# 
# Purpose: shows how to use the Preprocessor class to gather all the input
# data needed to create an HSPF model for an 8-digit hydrologic unit code 
# (HUC8), integrate the data to create an HSPFModel class for the HUC8,
# create a submodel for the gage for watershed associated with NWIS gage
# 05472500, calibrate the model for given parameters, and postprocess and 
# plot the results.

import os, pickle, datetime

from pyhspf import Postprocessor

# Paths to working directories for source NHDPlus, CDL, NWIS, NID datasets
# (modify as needed for the PC of interest)

network     = 'Z:'
destination = 'C:/HSPF_data'

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# NWIS gage for the calibration

gageid = '05472500'

# start and end dates for the model

start = datetime.datetime(1994, 1, 1)
end   = datetime.datetime(2009, 1, 1)

# warmup time (model output for this number of days is ignored)

warmup = 365

if not os.path.isdir(network):
    print('error, directory {} doesn not exist (please update)'.format(network))
    raise
if not os.path.isdir(destination):
    print('error, directory ' +
          '{} doesn not exist (please update)'.format(destination))
    raise

# Because parallel processing is (optionally) used, the process method has 
# to be called at runtime as shown below

if __name__ == '__main__': 

    # file path to place the calibrated model and results

    validation  = '{}/{}/validation'.format(destination, HUC8)

    if not os.path.isdir(validation): os.mkdir(validation)

    # path where the calibrated model will be saved/located

    calibrated = '{}/{}/calibration/{}'.format(destination, HUC8, gageid)

    # open the calibrated model

    with open(calibrated, 'rb') as f: hspfmodel = pickle.load(f)

    # get the comid of the gage

    d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
    comid = d[gageid]

    # change the filename

    its = destination, HUC8, gageid
    hspfmodel.filename = '{}/{}/validation/{}'.format(*its)

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
                                  output = '{}/hydrography'.format(validation))
    postprocessor.plot_calibration(output = '{}/statistics'.format(validation),
                                   show = False)
    postprocessor.plot_runoff(tstep = 'daily', show = False,
                              output = '{}/runoff'.format(validation))
    output = '{}/calibration_report.csv'.format(validation)
    postprocessor.calibration_report(output = output)
    postprocessor.plot_snow(output = '{}/snow'.format(validation), 
                            show = False)
    postprocessor.plot_dayofyear(output = '{}/dayofyear'.format(validation),
                                 show = False)
    postprocessor.plot_storms(season = 'all', show = False, 
                              output = '{}/storms'.format(validation))
