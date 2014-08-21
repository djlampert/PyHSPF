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
from pyhspf.preprocessing import climateutils
from pyhspf.preprocessing import PrecipStation
from pyhspf.preprocessing import EvapStation
from pyhspf.core          import WDMUtil
from pyhspf.core          import HSPFModel
from pyhspf.core          import Postprocessor
from pyhspf.calibration   import AutoCalibrator

# working directories to use for file extraction and processing

NWIS      = 'NWIS'                             # NWIS metadata files
NHDPlus   = 'NHDPlus'                          # NHDPlus source files
output    = 'HSPF_data'                        # output for the HUC8

# NHDPlus info for the Mid-Atlantic Region and the Patuxent River Basin

drainid   = 'MA'                          # NHDPlus Drainage Area ID
VPU       = '02'                          # NHDPlus Vector Processing Unit
HUC8      = '02060006'                    # 8-digit HUC
gage      = '01594670'                    # USGS Gage Site ID number
estart    = datetime.datetime(1988, 1, 1) # timeseries extraction start
eend      = datetime.datetime(2000, 1, 1) # timeseries extraction end

# names for extracted files for the watershed for HUC 02060006 (Patuxent River)

gagefile  = '{}/gagestations'.format(output)   # HUC8 gage station shapefile
flowfile  = '{}/flowlines'.format(output)      # HUC8 flowline shapefile
catchfile = '{}/catchments'.format(output)     # HUC8 catchment shapefile
bfile     = '{}/boundary'.format(output)       # HUC8 boundary shapefile
VAAfile   = '{}/flowlineVAAs'.format(output)   # NHDPlus value added attributes
elevfile  = '{}/elevations.tif'.format(output) # NED raster file
waterplot = '{}/NHDPlus.png'.format(output)    # plot of the HUC8 data
gagefiles = '{}/gagedata'.format(output)       # HUC8 NWIS flow data

# names for extracted files for the gage watershed (Hunting Creek)

gagepath  = '{}/{}'.format(output, gage)       # gage watershed files directory
gagedata  = '{}/{}'.format(gagepath, gage)     # gage daily flow data file
cfile     = '{}/catchments'.format(gagepath)   # gage catchment file
ffile     = '{}/flowlines'.format(gagepath)    # gage flowline file
masslink  = '{}/masslink.png'.format(gagepath) # gage mass linkage plot
watershed = '{}/watershed'.format(gagepath)    # gage Watershed file

# information for climate time series data extraction

bbox        = -77, 38.3, -76.4, 39.2  # bounding box to search for stations
evapstation = 'USC00180700'           # GHCND station for Beltsville, MD
precstation = '180465'                # Coop station number for BWI Airport, MD

# climate timeseries output file paths

beltsville = '{}/{}'.format(gagepath, evapstation)  # Beltsville GHCND data
bwi        = '{}/{}'.format(gagepath, precstation)  # BWI precipitation data

# working directory for HSPF files

hspf       = '{}/hspf'.format(output)
model      = '{}/hunting'.format(hspf)      # HSPF input and output files
start      = datetime.datetime(1988, 10, 1) # simulation start
end        = datetime.datetime(1990, 10, 1) # simulation end
calibrated = '{}/hspfmodel'.format(hspf)    # file path for the calibrated model

# land use data for the watershed (type and area in acres)

landuse = {'Developed':      239,
           'Forest':        8857,
           'Pasture/grass': 1554,
           'Agriculture':   1284
           }

def extract():
    """Create an extract function to call from at runtime and to turn off
    the extraction steps when they are done."""

    # create an instance of the NWIS extractor

    nwisextractor = NWISExtractor(NWIS)

    # download and decompress the source metadata files

    nwisextractor.download_metadata()

    # extract all the gage stations and metadata into a shapefile for the HUC8

    nwisextractor.extract_HUC8(HUC8, output)

    # tell the extractor to use the metadata file above to find gage data

    nwisextractor.set_metadata(gagefile)

    # create an instance of the NHDPlus extractor

    nhdplusextractor = NHDPlusExtractor(drainid, VPU, NHDPlus)

    # download and decompress the source data for the Mid Atlantic Region

    nhdplusextractor.download_data()

    # extract the HUC8 data for the Patuxent watershed

    nhdplusextractor.extract_HUC8(HUC8, output)

    # create an instance of the NHDPlusDelineator to use to build the Watershed

    delineator = NHDPlusDelineator(VAAfile, flowfile, catchfile, elevfile,
                                   gagefile = gagefile)

    # delineate the watershed (extract the flowlines, catchments and other data)

    delineator.delineate_gage_watershed(gage, output = gagepath)

    # download the daily flow and water quality data for the gage

    nwisextractor.download_gagedata(gage, estart, eend, output = gagedata)

    # open the NWIS flow data for the Hunting Creek gage station

    with open(gagedata, 'rb') as f: station = pickle.load(f)

    # get the time series of daily flow values for the gage

    flow = station.make_timeseries(estart, eend)

    # add land use data from 1988 to the delineator

    delineator.add_basin_landuse(1988, landuse)

    # build the watershed

    delineator.build_gage_watershed(gage, watershed, masslinkplot = masslink)

def climate():

    # find the GHCND evaporation stations and their metadata in the bounding box

    stations = climateutils.find_ghcnd(bbox, var = 'EVAP', types = 'GSN',
                                   dates = (estart, eend), verbose = True)

    # identify the Beltsville station from the list using the station name
 
    beltsville = stations[[s.station for s in stations].index(evapstation)]

    # download the GHCND data to the gage directory

    beltsville.download_data(gagepath, start = estart, end = eend, plot = True)

    # find the hourly precipitation metadata for the the bounding box

    stations = climateutils.find_precip3240(bbox, dates = (estart, eend), 
                                        verbose = True)

    # find the BWI Airport, MD station using the coop number

    bwi = stations[[s.coop for s in stations].index(precstation)]

    # download hourly precipitation data for BWI

    bwi.download_data(gagepath, estart, eend)

    # open the hourly precipitation station data for BWI Airport

    with open(bwi, 'rb') as f: precip3240 = pickle.load(f)

    # create a PyHSPF PrecipStation class instance to use to make a timeseries

    station = PrecipStation()

    # worth noting the PrecipStation is differentiated from the 
    # Precip3240Station to deal with things like missing data and to enable 
    # development of future tools to work with other datasets; this is true 
    # for other stations this is also helpful when parsing data for quality 
    # (not an issue here though)

    # add the precipitation data to the PyHSPF PrecipStation instance

    station.add_precip3240(precip3240)

    # make the timeseries and convert from in to mm

    precip = [p * 25.4 for p in station.make_timeseries(estart, eend)]

    # open the Beltsville GHCND station

    with open(beltsville, 'rb') as f: ghcnd = pickle.load(f) 

    # create a PyHSPF EvapStation class instance to use to make a timeseries

    station = EvapStation()

    # add the Beltsville data to the EvapStation instance

    station.add_ghcnd_data(ghcnd)

    # the evaporation data aren't collected in winter so just assume a fill 
    # value that is equal to the minimum observed

    fill = min([e for e in station.make_timeseries(estart, eend) 
                if e is not None])

    # make the daily time series

    evap = [fill if e is None else e 
            for e in station.make_timeseries(estart, eend)]

    # need hourly evaporation; disaggregate assuming constant throughout day

    evap = [e / 24 for e in evap for i in range(24)]

def calibrate():
    """Builds and calibrates the model."""

    # make the working directory for HSPF

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

    hunting.build_from_watershed(w, model, verbose = True)

    # turn on the hydrology modules to the HSPF model

    hunting.add_hydrology()

    # add precip timeseries with label BWI and provided start date to the model

    hunting.add_timeseries('precipitation', 'BWI', s, precip)

    # add evap timeseries with label Beltsville and provided start date 

    hunting.add_timeseries('evaporation', 'Beltsville', s, evap)

    # add flow timeseries with label Hunting, start date, tstep (days)

    #hunting.add_timeseries('flowgage', 'Hunting', start, flow, tstep = 1440)
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

    calibrator = AutoCalibrator(hunting, start, end, hspf)

    calibrator.autocalibrate(calibrated)

    for variable, value in zip(calibrator.variables, calibrator.values):

        print(variable, value)

    p = calibrator.hspfmodel.perlnds[0]

    print(p.LZSN, p.INTFW, p.UZSN, p.LZETP)

def postprocess():
    """Postprocess the results and save."""

    print('saving the calibration results\n')

    # open the calibrated model

    with open(calibrated, 'rb') as f: hunting = pickle.load(f)

    p = hunting.perlnds[0]

    print(p.LZSN, p.INTFW, p.UZSN, p.LZETP)

    # external targets for calibration

    targets = ['reach_outvolume', 'evaporation', 'groundwater', 'runoff', 
               'water_state']

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

# need to use the call to the main routine because this uses parallel cores

if __name__ == '__main__': 

    st = time.time()

    #extract()
    #climate()
    calibrate()
    postprocess()

    tot = time.time() - st

    print('finished extracting and calibrating the model for ' +
          '{} in {:f.1} seconds'.format(gage, tot))
