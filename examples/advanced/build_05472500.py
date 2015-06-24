# build_05472500.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 05/20/2015
# 
# Purpose: shows how to use the Preprocessor class to gather all the input
# data needed to create an HSPF model for an 8-digit hydrologic unit code 
# (HUC8), then integrate the data to create an HSPFModel class for the HUC8,
# then create a submodel for the gage for watershed associated with NWIS gage
# 05472500, then runs a baseline simulation and plots the results.

import os, pickle, datetime

# Paths to working directories for source NHDPlus, CDL, NWIS, NID datasets
# (modify as needed for the PC of interest)

if os.name == 'posix':
    network     = '/media/dave/DATA'
    destination = '/home/dave/HSPF_data'
elif os.name == 'nt':
    network     = 'D:'
    destination = 'C:/HSPF_data'

# import the Preprocessor and the Postprocessor

from pyhspf.preprocessing import Preprocessor
from pyhspf               import Postprocessor

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# two-digit state abbreviation for the CDL

state = 'ia'

# NWIS gage for the calibration

gageid = '05472500'

# start and end dates (2001 to 2010)

start = datetime.datetime(2001, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# maximum drainage area for subbasins in square kilometers

drainmax = 400

# Comma separated value file linking land use codes from the Cropland Data
# Layer to aggregated land use categories for HSPF land segments

aggregation = 'cdlaggregation.csv'

# Comma separated value file of parameters for the HSPF land use categories
# including RGB values for plots and evapotranspiration crop coefficients

landuse = 'lucs.csv'

# land use year to use for the model

landuseyear = 2001

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

    # open the model

    with open(processor.hspfmodel, 'rb') as f: hspfmodel = pickle.load(f)
    
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

    # make a dictionary that links the nwis gage to the comid

    nwis = {v:k for k,v in hspfmodel.subbasin_timeseries['flowgage'].items()}

    # get the comid of the nwis gage

    comid = nwis[gageid]

    # use the Postprocessor to analyze the results

    postprocessor = Postprocessor(hspfmodel, (start, end), comid = comid)

    postprocessor.get_hspexp_parameters()
    postprocessor.plot_hydrograph(tstep = 'monthly')
    postprocessor.plot_calibration()
    postprocessor.plot_runoff(tstep = 'daily')

# Using the preprocessor in other watersheds/gages *should* be as simple as
# supplying the parameters above (start and end date, state, 8-digit HUC, 
# NWIS gage ID, land use year, maximum drainage area); if you try and 
# get an error please report it!
