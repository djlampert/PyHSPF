# nwisexample.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 01/20/2015
#
# This example illustrates how to extract NWIS data for HSPF using the 
# NWISExtractor class. The extractor will download the source shapefile that
# has the metadata for the daily discharge and water quality data for the
# entire USA. If the source file already exists it will skip this step. 
# The example shows how to download all the data for a given HUC8, starting
# by extracting a point shapefile for the HUC8 then downloading the daily
# discharge and water quality data using the GageStation class (you can view
# the class in the preprocessing directory). The example then shows how 
# to download data for just one station as an alternative. Finally, the example
# shows how to open and work with the data for each gage in the watershed.

# base python module imports needed

import os, datetime, pickle

# pyhspf imports

from pyhspf.preprocessing import NWISExtractor

# paths for downloaded files

NWIS      = 'NWIS'       # location for NWIS metadata files
directory = 'data'       # working directory for input/output
HUC8      = '02060006'   # 8-digit HUC

# extracted files

gagepath  = '{}/gagedata'.format(directory)     # all HUC8 NWIS flow data path

# time series start and end

start = datetime.datetime(1980, 1, 1)      # start date for timeseries
end   = datetime.datetime(2010, 1, 1)      # end date for timeseries

# create an instance of the NWIS extractor and provide the location of the 
# metadata file (that is downloaded if it doesn't exist)

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

# the output directory will contain images with flow-duration curves and 
# daily flow hydrographs. the statements below show how to access the data 
# from the downloaded files. let's open up the data files from the patuxent 
# directory that was made above.

print('fetching flow data from the Patuxent watershed')

# make a list of all the data files just downloaded

datafiles = [f for f in os.listdir(gagepath) if f[-3:] != 'png']

# iterate through the list, get/print some data for each gage

print('Data for downloaded stations:')

# let's get the data for the year 2001 for each gage if it's available

s = datetime.datetime(2001, 1, 1)
e = datetime.datetime(2002, 1, 1)

for n in datafiles:

    p = '{}/{}'.format(gagepath, n)
    with open(p, 'rb') as f: station = pickle.load(f)

    # the following are attributes of the station directly from the database

    print('Gage ID:                     ', station.gageid)
    print('Name:                        ', station.name)
    print('State:                       ', station.state)
    print('First day of measurement:    ', station.day1)
    print('Last day of measurement:     ', station.dayn)
    print('Drainage area (square miles):', station.drain)
    print('Average flow (cfs):          ', station.ave)
    print('NWIS url:                    ', station.web)

    # get the time series of daily flow data from the start to end date
    # if it's available:

    try: 

        ts = station.make_timeseries(start = s, end = e)

        # if the start and end dates are not supplied, the function return the 
        # data for the whole period

        its = s.year, s.month, s.day, ts[0]
        print('flow on {:04d}-{:02d}-{:02d} (cfs):     {}'.format(*its))

        # calculate the average flow across the dates specified

        ave = sum(ts) / (e - s).days
        print('mean flow across dates (cfs): {:.1f}'.format(ave))

    except: pass

    # if the flow values are missing, the function will fill the value for 
    # the data with a "None"

    # any water quality data will be sporadic, so it is organized based on the
    # type of data into a dictionary with keys corresponding to NWIS codes 
    # and values that are a list of date/value pairs

    # the code for total suspended solids is 00530; the following shows how
    # to get the TSS data for a gage station

    print('')

    try:

        TSS = station.waterquality['00530']
        print('Number of suspended solids measurements:', len(TSS))
        print('TSS concentration on {}: {} mg/L'.format(*TSS[0]))

    except: 

        print('no TSS data available for this station')

    # a complete list of NWIS codes are available at
    # http://help.waterdata.usgs.gov/codes-and-parameters/parameters

    print('')
