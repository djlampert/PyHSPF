# example18.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the ClimateProcessor class to download climate data
# and estimate the potential evapotranspiration using the Penman-Monteith
# Equation, in hourly form consistent with the ASCE model.

import os, datetime

# import the ClimateProcessor that we will be using

from pyhspf.preprocessing import ClimateProcessor

# create an instance of the ClimateProcessor class--the previous climateutils 
# examples show how to download data, and the ClimateProcessor has pointers 
# to these methods essentially just to streamline the data download, 
# aggregation, and time series calculation processes.

processor = ClimateProcessor()

# working directory location for all the data files

output = 'HSPF_data'  

if not os.path.isdir(output): os.mkdir(output)

# datasets to download--this keyword argument can be either a list of the 
# particular datasets or 'all' (the default) which downloads all these: 
# (GHCND, GSOD, precip3240, NSRDB)

datasets = ['GHCND', 'GSOD', 'precip3240', 'NSRDB']

# bounding box of interest--let's use PyShp to get this from the boundary
# of the the Patuxent watershed, MD in the "data" folder

sf = 'data/patuxent/boundary'

if not os.path.isfile(sf + '.shp'):

    print('error: you seem to be missing the shapefile {}'.format(sf))
    raise

from shapefile import Reader

f = Reader(sf)

bbox = f.bbox

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# you may need to change the path to 7zip for your system; can also be done
# when the processor is instantiated (should not be necessary on linux)

processor.path_to_7z = r'C:/Program Files/7-Zip/7z.exe'

# download the data--worth noting there are some issues with the databases
# so there will be warnings, but this will give you ALL the data available
# so it will take a while and further processing will be needed.

processor.download(bbox, start, end, output, datasets = datasets)

# Still working on this one! just need to re-organize the processing...
