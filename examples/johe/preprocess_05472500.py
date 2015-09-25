# preprocess_05472500.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 08/10/2015
# 
# Purpose: shows how to use the Preprocessor class to gather all the input
# data needed to create an HSPF model for an 8-digit hydrologic unit code 
# (HUC8) and integrate the data to create an HSPFModel class for the HUC8,
# then runs the baseline model and saves/shows the simulation results.

import os, pickle, datetime

from pyhspf.preprocessing import Preprocessor
from pyhspf               import Postprocessor

# Paths to working directories for source NHDPlus, CDL, NWIS, NID datasets
# (modify as needed for the PC of interest)

network     = 'Z:'
destination = 'C:/HSPF_data'

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# NWIS gage for the calibration

gageid = '05472500'

# start and end dates for the model (1980 to 2011)

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

# make sure the directory and file paths exist

if not os.path.isdir(network):
    print('error, directory {} does not exist (please update)'.format(network))
    raise
if not os.path.isdir(destination):
    print('error, directory ' +
          '{} does not exist (please update)'.format(destination))
    raise
if not os.path.isfile(aggregation):
    print('error, file {} does not exist (please update)'.format(aggregation))
    raise
if not os.path.isfile(landuse):
    print('error, file {} does not exist (please update)'.format(landuse))
    raise

# Because parallel processing is (optionally) used, the process method has 
# to be called at runtime as shown below

if __name__ == '__main__': 

    # working directory for calibration simulations

    directory = '{}/{}/hspf'.format(destination, HUC8)

    # make an instance of the Preprocessor

    processor = Preprocessor()

    # set up the directory locations

    processor.set_network(network)
    processor.set_output(destination)

    # set the simulation-specific parameters

    processor.set_parameters(HUC8 = HUC8,
                             start = start,
                             end = end,
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
    #postprocessor.plot_runoff(tstep = 'daily', show = False,
    #                          output = '{}/runoff'.format(preliminary))
    output = '{}/calibration_report.csv'.format(preliminary)
    postprocessor.calibration_report(output = output)
    postprocessor.plot_snow(output = '{}/snow'.format(preliminary), 
                            show = False)
    postprocessor.plot_dayofyear(output = '{}/dayofyear'.format(preliminary),
                                 show = False)

