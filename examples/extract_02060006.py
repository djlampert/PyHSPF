# extract_02060006.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Extracts data for HSPF for HUC 02060006

# base python module imports needed

import os, datetime

# pyhspf imports

from pyhspf.preprocessing import *

# paths for downloaded files

NWIS      = '{}/NWIS'.format(os.getcwd())      # NWIS metadata files
NHDPlus   = '{}/NHDPlus'.format(os.getcwd())   # NHDPlus source files
#output    = '/home/dave/Desktop/HSPF_data'     # output for the HUC8
output    = 'HSPF_data'                        # output for the HUC8

# HUC8 info

drainid    = 'MA'                               # NHDPlus Drainage Area ID
VPU        = '02'                               # NHDPlus Vector Processing Unit
HUC8       = '02060006'                         # 8-digit HUC
gage       = '01594670'                         # USGS Gage Site ID number

# HUC8 extracted files

gagefile  = '{}/gagestations'.format(output)   # HUC8 gage station shapefile
flowfile  = '{}/flowlines'.format(output)      # HUC8 flowline shapefile
catchfile = '{}/catchments'.format(output)     # HUC8 catchment shapefile
bfile     = '{}/boundary'.format(output)       # HUC8 boundary shapefile
VAAfile   = '{}/flowlineVAAs'.format(output)   # NHDPlus value added attributes
elevfile  = '{}/elevations.tif'.format(output) # NED raster file
waterplot = '{}/NHDPlus.png'.format(output)    # plot of the HUC8 data
gagefiles = '{}/gagedata'.format(output)       # HUC8 NWIS flow data

# gage extracted files

watershed = '{}/{}'.format(output, gage)       # gage watershed files directory
gagedata  = '{}/{}'.format(watershed, gage)    # gage daily flow data file
cfile     = '{}/catchments'.format(watershed)  # gage catchment file
ffile     = '{}/flowlines'.format(watershed)   # gage flowline file

# time series info

start     = datetime.datetime(1980, 1, 1)      # start date for timeseries
end       = datetime.datetime(2010, 1, 1)      # end date for timeseries

# title for the plot

description = 'NHDPlus Catchments and Flowlines on 30 meter NED DEM'
title = 'Cataloging Unit %s\n%s' % (HUC8, description)

# land use data for the watershed (type and area in acres)

landuse = {'Developed':      239,
           'Forest':        8857,
           'Pasture/grass': 1554,
           'Cropland':      1284
           }

################# you shouldn't need to modify below this line #################

# create an instance of the NWIS extractor

nwisextractor = NWISExtractor(NWIS)

# download and decompress the source metadata files

nwisextractor.download_metadata()

# extract the gage stations for the HUC8

nwisextractor.extract_HUC8(HUC8, output)

# open the metadata for the HUC8 gages

nwisextractor.set_metadata(gagefile)

# download all the daily flow and water quality data from the gage shapefile

if not os.path.isdir(gagefiles):
    nwisextractor.download_all(start, end, output = gagefiles)

# create an instance of the NHDPlus extractor

nhdplusextractor = NHDPlusExtractor(drainid, VPU, NHDPlus)

# download and decompress the source data

nhdplusextractor.download_data()

# extract the HUC8 data for the Patuxent watershed

nhdplusextractor.extract_HUC8(HUC8, output)

# make a plot using the extracted files
   
if not os.path.isfile(waterplot):

    nhdplusextractor.plot_HUC8(flowfile, catchfile, bfile, VAAfile, elevfile,
                               title = title, output = waterplot)

# use the delineator to build the HSPF model

delineator = NHDPlusDelineator(VAAfile, flowfile, catchfile, elevfile,
                               gagefile = gagefile)

# delineate the watershed

delineator.delineate_gage_watershed(gage, output = watershed)

# download the daily flow and water quality data for the gage into the directory

nwisextractor.download_gagedata(gage, start, end, output = gagedata)

# add land use data to the delineator

delineator.add_basin_landuse(landuse)

# build the watershed

delineator.build_gage_watershed(gage)
