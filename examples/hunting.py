# extract_02060006.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Extracts data for HSPF for HUC 02060006

# base python module imports needed

import os, datetime

# pyhspf imports

from pyhspf.preprocessing import *
from pyhspf               import HSPFModel

# working directories to use for file extraction and processing

NWIS      = 'NWIS'                              # NWIS metadata files
NHDPlus   = 'NHDPlus'                           # NHDPlus source files
output    = 'HSPF_data'                         # output for the HUC8

# NHDPlus info for the Mid-Atlantic Region and the Patuxent River Basin

drainid    = 'MA'                               # NHDPlus Drainage Area ID
VPU        = '02'                               # NHDPlus Vector Processing Unit
HUC8       = '02060006'                         # 8-digit HUC
gage       = '01594670'                         # USGS Gage Site ID number

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
watershed = '{}/watershed'.format(gagepath)    # gage Watershed class instance

# information for climate time series data extraction

bbox        = -77, 38.3, -76.4, 39.2         # search bounding box for stations
stateFIPS   = '18'                           # FIPS number for state of MD
evapstation = 'USC00180700'                  # GHCND station for Beltsville, MD
ghcndfile   = '{}/ghcnd'.format(gagepath)    # GHCND data output file name
stateprec   = '{}/prec3240'.format(gagepath) # MD hourly precipitation output
precstation = '180465'                       # coop no. for BWI Airport, MD
#precfile    = '{}/prec3240'.format(gagepath) # hourly precipitation output file

# time series info

start     = datetime.datetime(1988, 10, 1)      # start date for timeseries
end       = datetime.datetime(1990, 10, 1)      # end date for timeseries

# land use data for the watershed (type and area in acres)

landuse = {'Developed':      239,
           'Forest':        8857,
           'Pasture/grass': 1554,
           'Agriculture':   1284
           }

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

nwisextractor.download_gagedata(gage, start, end, output = gagedata)

# add land use data from 1988 to the delineator

delineator.add_basin_landuse(1988, landuse)

# build the watershed

delineator.build_gage_watershed(gage, watershed, masslinkplot = masslink)

# find the GHCND evaporation stations (and their metadata) in the bounding box

stations = climateutils.find_ghcnd(bbox, var = 'EVAP', types = 'GSN',
                                   dates = (start, end), verbose = True)

# identify the Beltsville station from the list using the station name
 
beltsville = stations[[s.station for s in stations].index(evapstation)]

# download the GHCND data to the gage directory

beltsville.download_data(gagepath, start = start, end = end, plot = True)

# find the hourly precipitation metadata for the the bounding box

stations = climateutils.find_precip3240(bbox, dates = (start, end), 
                                        verbose = True)

# find the BWI Airport, MD station using the coop number

bwi = stations[[s.coop for s in stations].index(precstation)]

# download hourly precipitation data for BWI

bwi.download_data(gagepath, start, end)

# use the data to build an HSPFModel

#hspfmodel = HSPFModel(units = 'Metric')

