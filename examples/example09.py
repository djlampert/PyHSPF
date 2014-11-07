# example09.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Shows how to use the NHDPlusDelineator to delineate the watershed for a gage
# within a HUC8. Assumes the NHDPlus Hydrography data and gage station 
# shapefile for the HUC8 exist already.

# base python module imports needed

import os, datetime

# pyhspf imports

from pyhspf.preprocessing import NHDPlusDelineator

# paths for downloaded files

output = 'HSPF_data'     # output for the HUC8

# HUC8 info

HUC8       = '02060006'                         # 8-digit HUC
gage       = '01594670'                         # USGS Gage Site ID number

# extracted files

gagefile  = '{}/gagestations'.format(output)   # HUC8 gage station shapefile
flowfile  = '{}/flowlines'.format(output)      # HUC8 flowline shapefile
cfile     = '{}/catchments'.format(output)     # HUC8 catchment shapefile
bfile     = '{}/boundary'.format(output)       # HUC8 boundary shapefile
VAAfile   = '{}/flowlineVAAs'.format(output)   # NHDPlus value added attributes
elevfile  = '{}/elevations.tif'.format(output) # NED raster file
watershed = '{}/{}'.format(output, gage)       # gage watershed files directory

# create an instance of the delineator and supply the path to the source files

delineator = NHDPlusDelineator(VAAfile, flowfile, cfile, elevfile,
                               gagefile = gagefile)

# extracts the catchments and flowlines for the gage's watershed and plot it

delineator.delineate_gage_watershed(gage, output = watershed)
