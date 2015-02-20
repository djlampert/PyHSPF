# climateprocessor02.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 02/15/2015
#
# illustrates how to use the ClimateProcessor class to download and shows 
# how to aggregate climate time series using some numpy features. The first
# part is the same as the previous example, so there is minimal discussion.

import os, datetime, pickle

# import the ClimateProcessor

from pyhspf.preprocessing import ClimateProcessor

# create an instance of the ClimateProcessor class

processor = ClimateProcessor()

# working directory location for all the data files

output = 'HSPF_data'  

if not os.path.isdir(output): os.mkdir(output)

# this is the bounding box of interest from the last example

bbox = -77.2056, 38.2666, -76.4008, 39.3539

# start and end dates (just look at one year to highlight the concept)

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(1981, 1, 1)

# path to 7zip if using windows

processor.path_to_7z = r'C:/Program Files/7-Zip/7z.exe'

# download the data; this step will be skipped if this has already been done
# (from the last example) but is necessary to set the metadata

processor.download(bbox, start, end, output)

# let's use GHCND to work with temperature data. let's use the metadata to 
# find the 10 stations with the longest temperature records and then aggregate 
# the data together to get a mean time series across the period of interest.

n = 10

# make a list of the 
templengths = []

for k, v in processor.metadata.ghcndstations.items(): 

    templengths.append((v['tmax'], k))

# sort the list; the last 10 values are the longest records

templengths.sort()

tempstations = [k for v, k in templengths[-n:]]

# let's print out the station names, and fetch the time series for processing

for k in tempstations:

    m = processor.metadata.ghcndstations[k]

    its = m['name'], m['tmax']

    print('station: {:24s} {} temperature observations'.format(*its))

print('')

# the time step size is a day (GHCND is a daily database)

delta = datetime.timedelta(days = 1)

# make a list of times

times = [start + i * delta for i in range((end - start).days)]

# get the data and store it in a numpy array. this gets computationally 
# expensive so it's wise to take advantage of these built-in modules. 

import numpy

# pre-allocate space in an empty array

tmax = numpy.empty((n, len(times)))

# iterate through the sample and fill in the array

for i, k in enumerate(tempstations):

    print('fetching the temperature time series data from {}\n'.format(k))

    # open up the GHCNDStation data

    with open(k, 'rb') as f: s = pickle.load(f)

    # make the time series and fill it in--one thing to note is that this 
    # method includes Nones if data are missing--this is really critical and
    # can be a huge pain since a complete, continuous time series is needed
    # to run simulations

    tmax[i,:] = s.make_timeseries('tmax', start = start, end = end)

# let's transpose to switch the ordering from stations to days

tmax = tmax.transpose()

# let's take advantage of numpy to analyze the data. the following lines show
# how to look for None or nan (missing values)--numpy uses "masked arrays" 
# that can help with this

nones = numpy.equal(tmax, None)
nans  = numpy.isnan(tmax)

print('number of missing values:', nones.size + nans.size)

# this creates a Boolean array (true/false) to "mask" the None and nan values
# the | (pipe) is the equivalent of "or"

mask = numpy.invert(nones | nans)

# go through the days and get the average, min, and max

mean, lower, upper = [], [], []
for row, m in zip(tmax, mask): 
    mean.append(row[m].mean())
    lower.append(row[m].min())
    upper.append(row[m].max())

print(min(mean), min(lower), min(upper))

print(sum(mean) / len(mean), sum(lower) / len(lower), sum(upper) / len(upper))
 
# this example was made simply to show one way to work with the data--PyHSPF
# has built-in methods to do the aggregation steps above more simply. however, 
# because there are an infinite number of different ways to work with the 
# climate data for HSPF, the idea is to create a development environment for
# new tools. HSPF is typically run with data from a single station, but there
# is really no need for this assumption. in general, the temperature values
# are not so critical for HSPF, so the most logical approach to me is to
# average the largest "n" datasets together to account for spatial variability,
# missing data, and reporting errors.



