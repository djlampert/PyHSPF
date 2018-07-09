# climateprocessor01.py
#
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the ClimateProcessor class to download climate data
# and how to view the metadata for the various databases available in PyHSPF.

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

sf = 'subbasin_catchments'

if not os.path.isfile(sf + '.shp'):

    print('\nerror: you seem to be missing the {} shapefile\n'.format(sf))
    raise

from shapefile import Reader

f = Reader(sf)

bbox = f.bbox

# start and end dates for time series extraction

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# you may need to change the path to 7zip for your system; can also be done
# when the processor is instantiated (should not be necessary on linux)

processor.path_to_7z = r'C:/Program Files/7-Zip/7z.exe'

# download the data--worth noting there are some issues with the databases
# sometimes. these are handled with warnings, but this will give you ALL the
# data available so it takes a while (obviously longer the bigger the area
# requested) and further processing will be needed to develop "final" datasets
# for a model simulation. if the subdirectories for the various databases in
# the output directory already exist (i.e., if you have already done the data
# download), this step will be skipped but it will  tell the processor the
# location of the data files and read the metadata for further processing.

processor.download(bbox, start, end, output, datasets = datasets)

# because there are so many data files, it makes sense to keep track of some
# metadata for the files to use for parsing, etc. the following lines show
# how the metadata are organized. the filenames are dictionary keys, and the
# values are dictionaries containing the name of the station, the latitude,
# the longitude, the elevation, and the length of the datasets.

# GHCND (daily precip, tmax, tmin, snowdepth, snowfall, wind, pan evaporation

for k, v in processor.metadata.ghcndstations.items():

    print('\n'.join(['{} {} {}'.format(k, p, val) for p, val in v.items()]))

# GSOD (daily precip, tmax, tmin, wind, dewpoint)

for k, v in processor.metadata.gsodstations.items():

    print('\n'.join(['{} {} {}'.format(k, p, val) for p, val in v.items()]))

# hourly precipitation (only one dataset class)

for k, v in processor.metadata.precip3240stations.items():

    print('\n'.join(['{} {} {}'.format(k, p, val) for p, val in v.items()]))

# NSRDB (legacy data (pre-1991), METSTAT model, SUNY model, observed (rare))

for k, v in processor.metadata.nsrdbstations.items():

    print('\n'.join(['{} {} {}'.format(k, p, val) for p, val in v.items()]))
