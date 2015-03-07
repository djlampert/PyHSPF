# climateprocessor07.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 03/06/2015
#
# this example illustrates how to use the ClimateProcessor class to aggregate 
# raw precipitation data into subbasin-specific time series using an inverse 
# distance weighted average. makes use of the "subbasin_catchments" shapefile
# in this directory that was generated using the NHDPlusExtractor and 
# HUC8Delineator classes shown elsewhere.

import os, datetime, pickle, numpy

# import the ClimateProcessor and PyShp reader

from pyhspf.preprocessing import ClimateProcessor
from shapefile            import Reader

# path to existing shapefile defining the data region and processing information

filename = 'subbasin_catchments'

if not os.path.isfile(filename + '.shp'):
    print('error, {} does not exist!'.format(filename))
    raise

# create an instance of the ClimateProcessor class

processor = ClimateProcessor()

# working directory location for all the data files

output = 'HSPF_data'  

if not os.path.isdir(output): os.mkdir(output)

# working directory for aggregated precipitation files

directory = '{}/subbasinprecipitation'.format(output)

if not os.path.isdir(directory): os.mkdir(directory)

# start and end dates (aggregate the whole 31 years)

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# the space argument increases the bounding box area for the data download and
# processing; let's use a larger area for this example to grab data from a 
# few more stations

space = 0.5

# download/set the location of the data using the "download_shapefile" method

processor.download_shapefile(filename, start, end, output, 
                             datasets = ['precip3240'], space = 0.5)

# open up and package the time series and locations for processing later

names, lons, lats = [], [], []

# make an empty numpy array for the data

precipitations = numpy.empty((len(processor.metadata.precip3240stations),
                             (end-start).days * 24))

i = 0
for k,v in processor.metadata.precip3240stations.items():

    with open(k, 'rb') as f: station = pickle.load(f)

    names.append(v['name'])
    lons.append(v['longitude'])
    lats.append(v['latitude'])

    precipitations[i,:] = station.make_timeseries('precip3240', start, end)
    i += 1

# transpose the list to iterate by timestep rather than by station

precipitations = precipitations.transpose()

# this example calculates a time series for each shape in the 
# subbasin_catchments shapefile using the inverse distance weighted average 
# of the precipitation stations from the centroid of each subbasin. the file 
# was generated using the NHDPlusExtractor and the HUC8Delineator to create 
# catchments for the Patuxent watershed (HUC 02060006). each shape is a 
# catchment subbasin with a Common Identifier (ComID) feature attribute that 
# corresponds to the NHDPlus ComID for the subbasin reach outlet, a CenX 
# feature attribute representing the longitude of the centroid, and a CenY
# feature attribute representing the latitude of the centroid. let's open the 
# shapefile and get the centroids.

sf = Reader(filename)

# index of the comid, latitude, and longitude records

comid_index = [f[0] for f in sf.fields].index('ComID') - 1
lon_index   = [f[0] for f in sf.fields].index('CenX')  - 1
lat_index   = [f[0] for f in sf.fields].index('CenY')  - 1

# iterate through the shapefile records and make the timeseries

for i in range(len(sf.records())):

    record = sf.record(i)
    comid  = record[comid_index]
    lon    = record[lon_index]
    lat    = record[lat_index]

    i = comid, lon, lat
    print('aggregating timeseries for comid {} at {}, {}\n'.format(*i))

    # create weighting factors using the inverse-distance squared
    
    weights = []

    # get the distances between each station and the centroid

    for name, x, y in zip(names, lons, lats):

        point = x,y

        # the processor has "get_distance" method to estimate the distance in 
        # kilometers between two points given as (lon1, lat1), (lon2, lat2)

        distance = processor.get_distance(point, (lon, lat))

        print('the distance to station {} is {:.1f} km'.format(name, distance))

        weights.append(1 / distance**2)

    # convert weights to a numpy array

    weights = numpy.array(weights)

    # generate a mask array to hide missing values (as shown previously)

    mask = numpy.invert(numpy.isnan(precipitations))

    # go through each hour and get the weighted average of the available values

    precipitation = []
    for row, ma in zip(precipitations, mask):

        # mask the weighting factors

        w = weights[ma]

        # mask the precipitation values

        p = row[ma]

        # calculate the weighted average

        precipitation.append((w * p).sum() / w.sum())

    mean = sum(precipitation) / (end - start).days * 365.25

    print('\naggregated annual average precipitation: {:.1f} in\n'.format(mean))

    # dump the result in PyHSPF timeseries format into a pickle file for later

    ts = start, 60, precipitation

    # use the unique common identifiers for the files

    p = '{}/{}'.format(directory, comid)
    with open(p, 'wb') as f: pickle.dump(ts, f)

i = filename, directory
print('done aggregating all timeseries in {}, results in {}\n'.format(*i))

