# example10.py
#
# David J. Lampert (djlampert@gmail.com)
#
# This example illustrates how to extract NWIS data for HSPF using the 
# NWISExtractor class. The extractor will download the source shapefile that
# has the metadata for the daily discharge and water quality data for the
# entire USA. If the source file already exists it will skip this step. 
# The example shows how to download all the data for a given HUC8, starting
# by extracting a point shapefile for the HUC8 then downloading the daily
# discharge and water quality data using the GageStation class (you can view
# the class in the preprocessing directory). The example then shows how 
# to download data for just one station as an alternative.

# base python module imports needed

import os, datetime

# pyhspf imports

from pyhspf.preprocessing import NWISExtractor

# paths for downloaded files

NWIS      = 'NWIS'           # location for NWIS metadata files
directory = 'data/patuxent'  # working directory for input/output
HUC8      = '02060006'       # 8-digit HUC

# extracted files

gagepath  = '{}/gagedata'.format(directory)     # all HUC8 NWIS flow data path

# time series start and end

start = datetime.datetime(1980, 1, 1)      # start date for timeseries
end   = datetime.datetime(2010, 1, 1)      # end date for timeseries

# create an instance of the NWIS extractor

nwisextractor = NWISExtractor(NWIS)

# extract the locations of the gage stations in the HUC8 (Patuxent watershed) 
# into a new shapefile 

nwisextractor.extract_HUC8(HUC8, directory)

# and download all the daily flow and water quality data across the period 
# for the gages in the gage shapefile

if not os.path.isdir(gagepath):
    nwisextractor.download_all(start, end, output = gagepath)

# alternatively, download the daily flow and water quality data for one gage
# given the USGS NWIS Site ID number (Hunting Creek)

gageid    = '01594670'
gagedata  = 'hunting_station'
nwisextractor.download_gagedata(gageid, start, end, output = gagedata)
