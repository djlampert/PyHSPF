# hunting.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Illustrates how to use PyHSPF to build a calibrated HSPF model for the
# Hunting Creek watershed in the Patuxent Basin, MD. Details throughout.
# Extending this example to other watersheds should be as easy as changing 
# a few parameters below--if you attempt this please let me know if it works.

# base python module imports needed

import os, pickle, datetime, time

# pyhspf imports

from pyhspf.preprocessing import NWISExtractor
from pyhspf.preprocessing import NHDPlusExtractor
from pyhspf.preprocessing import NHDPlusDelineator
from pyhspf.core          import WDMUtil
from pyhspf.core          import HSPFModel
from pyhspf.core          import Postprocessor
from pyhspf.calibration   import AutoCalibrator

# source directory for NHDPlus and NWIS data (I use a network mapped to Z:)

source = 'Z:'

# destination for extracted files for various HUC8 watershed data and models

destination = 'HSPF_data'

if not os.path.isdir(destination): os.mkdir(destination)

# NHDPlus info for the Mid-Atlantic Region and the Patuxent River Basin

VPU       = '02'        # NHDPlus Vector Processing Unit
HUC8      = '02060006'  # 8-digit HUC
gageid    = '01594670'  # USGS Gage Site ID number

# path to the working directory for th HUC8

output = '{}/{}'.format(destination, HUC8)

# working directories to use for file extraction and processing

NWIS    = '{}/NWIS'.format(source)      # NWIS metadata files
NHDPlus = '{}/NHDPlus'.format(source)   # NHDPlus source files

# names for extracted files for the watershed for HUC 02060006 (Patuxent River)

gagefile  = '{}/gagestations'.format(output)    # gage station shapefile
flowfile  = '{}/flowlines'.format(output)       # flowline shapefile
catchfile = '{}/catchments'.format(output)      # catchment shapefile
bfile     = '{}/boundary'.format(output)        # boundary shapefile
VAAfile   = '{}/flowlineVAAs'.format(output)    # VAA file
elevfile  = '{}/elevations.tif'.format(output)  # NED raster file
waterplot = '{}/NHDPlus.png'.format(output)     # plot of the HUC8 data
gagefiles = '{}/gagedata'.format(output)        # HUC8 NWIS flow data

# names for extracted files for the gage watershed (Hunting Creek)

gagepath  = '{}/{}'.format(output, gageid)     # gage watershed files directory
gagedata  = '{}/{}'.format(gagepath, gageid)   # gage daily flow data file
cfile     = '{}/catchments'.format(gagepath)   # gage catchment file
ffile     = '{}/flowlines'.format(gagepath)    # gage flowline file
masslink  = '{}/masslink.png'.format(gagepath) # gage mass linkage plot
watershed = '{}/watershed'.format(gagepath)    # gage Watershed file

# working directory for HSPF files

hspf       = '{}/hspf'.format(output)
model      = '{}/hunting'.format(hspf)      # HSPF input and output files
start      = datetime.datetime(1988, 10, 1) # simulation start
end        = datetime.datetime(1990, 10, 1) # simulation end
calibrated = '{}/hspfmodel'.format(hspf)    # file path for the calibrated model

# HSPF calibration methodology
    
#variables     = {'LZSN':   0.70,  # starting value relative to PyHSPF default
#                 'UZSN':   6.1,   # starting value relative to PyHSPF default
#                 'LZETP':  0.4,   # starting value relative to PyHSPF default
#                 'INFILT': 0.90,  # starting value relative to PyHSPF default
#                 'INTFW':  0.20,  # starting value relative to PyHSPF default
#                 'AGWRC':  1.01,  # starting value relative to PyHSPF default
#                 'IRC':     0.5,  # starting value relative to PyHSPF default
#                 }
variables     = {'LZSN':   0.725,  # starting value relative to PyHSPF default
                 'UZSN':   6.75,   # starting value relative to PyHSPF default
                 'LZETP':  0.22,   # starting value relative to PyHSPF default
                 'INFILT': 1.34,  # starting value relative to PyHSPF default
                 'INTFW':  0.20,  # starting value relative to PyHSPF default
                 'AGWRC':  1.004,  # starting value relative to PyHSPF default
                 'IRC':     0.5,  # starting value relative to PyHSPF default
                 }
optimization  = 'Nash-Sutcliffe Product'  # optimization parameter
perturbations = [2, 1, 0.5]               # degree of perturbation
parallel      = True                      # parallel flag

# land use data for the watershed (type & area, only correct fractions needed)

landuse = {'Developed':      119,
           'Forest':        4428,
           'Pasture/grass':  777,
           'Agriculture':    642
           }

def preprocess():

    # create an instance of the NWIS extractor

    nwisextractor = NWISExtractor(NWIS)

    # download and decompress the source metadata files

    nwisextractor.download_metadata()

    # extract all the gage stations and metadata into a shapefile for the HUC8

    nwisextractor.extract_HUC8(HUC8, output)

    # create an instance of the NHDPlus extractor

    nhdplusextractor = NHDPlusExtractor(VPU, NHDPlus)

    # download and decompress the source data for the Mid Atlantic Region

    nhdplusextractor.download_data()

    # extract the HUC8 data for the Patuxent watershed

    nhdplusextractor.extract_HUC8(HUC8, destination)

    # create an instance of the NHDPlusDelineator to use to build the Watershed

    delineator = NHDPlusDelineator(VAAfile, flowfile, catchfile, elevfile,
                                   gagefile = gagefile)

    # delineate the watershed (extract the flowlines, catchments and other data)

    delineator.delineate_gage_watershed(gageid, output = gagepath)

    # add land use data from 1988 to the delineator

    delineator.add_basin_landuse(1988, landuse)

    # build the watershed

    delineator.build_gage_watershed(gageid, watershed, masslinkplot = masslink)

    # make the working directory for HSPF simulation files

    if not os.path.isdir(hspf): os.mkdir(hspf)

    # import old data for Hunting Creek

    wdm = WDMUtil()

    # path to hspexp2.4 data files (modify as needed)

    directory = os.path.abspath(os.path.dirname(__file__)) + '/data'

    # the data from the export file (*.exp) provided with hspexp need to be 
    # imported into a wdm file. WDMUtil has a method for this.

    hunthour = '{}/hunthour/huntobs.exp'.format(directory)

    f = 'temp.wdm'

    # import from exp to wdm

    wdm.import_exp(hunthour, f)

    # close the file and re-open the wdm for read access

    wdm.close(f)
    wdm.open(f, 'r')

    # the dsns are known from the exp file so just use those this time

    precip = wdm.get_data(f, 106)
    evap   = wdm.get_data(f, 111)
    flow   = wdm.get_data(f, 281)

    s, e = wdm.get_dates(f, 106)

    # add the time series to deal with HSPF looking backward stepping

    precip = [0] + [p * 25.4 for p in precip]
    evap   = [e * 25.4 / 24 for e in evap for i in range(24)]

    wdm.close(f)

    # create an HSPF model instance

    hunting = HSPFModel()

    # open the watershed built above

    with open(watershed, 'rb') as f: w = pickle.load(f)

    # use the data to build an HSPFModel

    hunting.build_from_watershed(w, model, ifraction = 1., verbose = True)

    # turn on the hydrology modules to the HSPF model

    hunting.add_hydrology()

    # add precip timeseries with label BWI and provided start date to the model

    hunting.add_timeseries('precipitation', 'BWI', s, precip)

    # add evap timeseries with label Beltsville and provided start date 

    hunting.add_timeseries('evaporation', 'Beltsville', s, evap)

    # add flow timeseries with label Hunting, start date, tstep (days)

    hunting.add_timeseries('flowgage', 'Hunting', s, flow, tstep = 60)

    # assign the evaporation and precipiation timeseries to the whole watershed

    hunting.assign_watershed_timeseries('precipitation', 'BWI')
    hunting.assign_watershed_timeseries('evaporation', 'Beltsville')

    # find the subbasin indentfier for the watershed outlet

    subbasin = [up for up, down in w.updown.items() if down == 0][0]

    # assign the flowgage to the outlet subbasin

    hunting.assign_subbasin_timeseries('flowgage', subbasin, 'Hunting')

    # using pan evaporation data, so need a pan coefficient < 1

    hunting.evap_multiplier = 0.75

    with open(calibrated, 'wb') as f: pickle.dump(hunting, f)

def calibrate():

    with open(calibrated, 'rb') as f: hunting = pickle.load(f)

    calibrator = AutoCalibrator(hunting, start, end, hspf)

    calibrator.autocalibrate(calibrated,
                             variables = variables, 
                             optimization = optimization,
                             perturbations = perturbations,
                             parallel = parallel
                             )

    for variable, value in zip(calibrator.variables, calibrator.values):

        print('{:6s} {:5.3f}'.format(variable, value))

    print('\nsaving the calibration results\n')

def postprocess():

    # open the calibrated model

    with open(calibrated, 'rb') as f: hunting = pickle.load(f)

    # external targets for calibration

    targets = ['reach_outvolume', 
               'evaporation', 
               'groundwater', 
               'runoff', 
               'water_state'
               ]

    # build the HSPF input files

    hunting.build_wdminfile()
    hunting.build_uci(targets, start, end, hydrology = True)

    # run it

    hunting.run(verbose = True)

    # create a postprocessor to analyze the results

    postprocessor = Postprocessor(hunting, (start, end))

    # make a report of the calibration

    report = '{}/calibration_report.csv'.format(hspf)
    postprocessor.get_hspexp_parameters(verbose = False)
    postprocessor.get_calibration()
    postprocessor.calculate_errors(output = report, verbose = False)

    # plot the daily hydrograph

    hydrograph = '{}/dailyhydrograph'.format(hspf)
    postprocessor.plot_hydrograph(tstep = 'daily', output = hydrograph, 
                                  show = False)

    # plot the monthly hydrograph

    hydrograph = '{}/monthlyhydrograph'.format(hspf)
    postprocessor.plot_hydrograph(tstep = 'monthly', output = hydrograph, 
                                  show = False)

    # plot the calibration

    calibration_graph = '{}/calibration'.format(hspf)
    postprocessor.plot_calibration(output = calibration_graph, show = False)

    # storm events from HSPExp file (for plotting)

    storms = [[datetime.datetime(1989, 2, 20), datetime.datetime(1989, 2, 26)],
              [datetime.datetime(1989, 5,  1), datetime.datetime(1989, 5,  5)],
              [datetime.datetime(1989, 6,  6), datetime.datetime(1989, 6, 11)],
              [datetime.datetime(1989, 9, 25), datetime.datetime(1989, 9, 29)],
              [datetime.datetime(1989,10,  1), datetime.datetime(1989,10,  5)],
              [datetime.datetime(1989,12, 31), datetime.datetime(1990, 1,  5)],
              [datetime.datetime(1990, 1, 25), datetime.datetime(1990, 1, 30)],
              [datetime.datetime(1990, 5,  4), datetime.datetime(1990, 5,  8)],
              [datetime.datetime(1990, 5, 28), datetime.datetime(1990, 6,  1)],
              [datetime.datetime(1990, 6, 15), datetime.datetime(1990, 6, 18)],
              ]

    # plot the storm events

    storm_plots = '{}/storms'.format(hspf)
    postprocessor.plot_storms(stormdates = storms, output = storm_plots, 
                              tstep = 'hourly', show = False)

# need to use the call to the main routine because this uses parallel cores

if __name__ == '__main__': 

    st = time.time()

    #preprocess()
    #calibrate()
    postprocess()
    
    tot = time.time() - st

    print('finished extracting and calibrating the model for ' +
          '{} in {:.1f} seconds'.format(gageid, tot))
