# example17.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the preprocessing tools to download climate data
# needed to estimate the potential evapotranspiration and perform a snow 
# simulation. similar to the last examples but shows other climate data
# extraction tools that are available.

from pyhspf.preprocessing import climateutils

import os, pickle, datetime

# bounding box of interest

bbox = -94, 41.3, -91.3, 42.5 

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)
dates = start, end

# download all the GHCND data (this includes daily values of tmin, tmax, 
# wind, snow, precipitation, and pan evaporation)

stations = climateutils.find_ghcnd(bbox, dates = dates, verbose = True)

# download the GHCND data to "output" location

output = 'GHCND'
if not os.path.isdir(output): os.mkdir(output)

for station in stations: 

    if not os.path.isfile('{}/{}'.format(output, station.station)):

        station.download_data(output, start = start, end = end, plot = True)



# download all the GSOD data (this includes daily values of tmin, tmax, 
# wind, precipitation, and dew point)

stations = climateutils.find_gsod(bbox, dates = dates, verbose = True)

# download the data to "output" location

output = 'GSOD'
if not os.path.isdir(output): os.mkdir(output)

for station in stations: 

    var = output, station.airforce, station.wban
    destination = '{0}/{1:06d}-{2:05d}'.format(*var)

    if not os.path.isfile(destination):

        station.download_data(output, start = start, end = end, plot = True)



# download all the hourly precipitation data from NCDC Precipn 3240 dataset

stations = climateutils.find_precip3240(bbox, dates = dates, verbose = True)

# download the data to "output" location

output = 'hourlyprecip'
if not os.path.isdir(output): os.mkdir(output)

# the path to 7zip to decompress the archives

path_to_7z = r'C:/Program Files/7-Zip/7z.exe'

# download/import the data for each station; the data on the web are grouped
# by state and by year, so the same files are used for each station

for station in stations:

    if not os.path.isfile('{}/{}'.format(output, station.coop)):
        
        station.download_data(output, start, end, path_to_7z = path_to_7z,
                              clean = False, plot = True)



# download all the solar radiation data from NREL's NSRDB

stations = climateutils.find_nsrdb(bbox, dates = dates, verbose = True)

# download the data to "output" location

output = 'NSRDB'
if not os.path.isdir(output): os.mkdir(output)

for station in stations: 

    if not os.path.isfile('{}/{}'.format(output, station.usaf)):

        station.download_data(output, dates = (start, end))

