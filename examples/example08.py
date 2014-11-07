# example08.py
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

NWIS      = 'NWIS'         # location for NWIS metadata files
directory = 'HSPF_data'    # working directory for input/output
HUC8      = '02060006'     # 8-digit HUC
gage      = '01594670'     # USGS Gage Site ID number

# extracted files

output    = '{}/{}'.format(directory, HUC8)    # output directory for the HUC8
gagepath  = '{}/gagedata'.format(output)       # all HUC8 NWIS flow data path
gagedata  = 'hunting_station'                  # data file for one gage

# time series start and end

start = datetime.datetime(1980, 1, 1)      # start date for timeseries
end   = datetime.datetime(2010, 1, 1)      # end date for timeseries

# create an instance of the NWIS extractor

nwisextractor = NWISExtractor(NWIS)

# extract the gage stations into a new shapefile for the HUC8

nwisextractor.extract_HUC8(HUC8, output)

# download all the daily flow and water quality data from the gage shapefile

if not os.path.isdir(gagepath):
    nwisextractor.download_all(start, end, output = gagepath)

# download the daily flow and water quality data for one gage

nwisextractor.download_gagedata(gage, start, end, output = gagedata)
