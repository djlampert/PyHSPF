# example12.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the preprocessing tools to download climate data.
#
# last updated: 01/10/2015

from pyhspf.preprocessing import climateutils

import os, pickle, datetime

# bounding box of interest

bbox = -94, 41.3, -91.3, 42.5 

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# this was (at the time of writing) the url to the NCDC database for the GHCND:
#
# GHCND = 'http://www1.ncdc.noaa.gov/pub/data/ghcn/daily'
#
# if the site changes this can be updated

# find the stations with evaporation data and make a list of GHCNDStation 
# instances to use to download the data for the stations

# GHCND variable to look for data ('EVAP' is pan evaporation--look at the
# website if you are interested in other variables)

var = 'EVAP'

# find the stations with EVAP data

stations = climateutils.find_ghcnd(bbox, var = var, dates = (start, end), 
                                   verbose = True)

# download the data to "output" location (here it's the current directory, ".")
# each station will be saved as a binary file with its unique GHCND ID

output = '.'

for station in stations: station.download_data(output, start = start, end = end,
                                               plot = False)

# there are 4 files in the bounding box (you wouldn't know this in advance); 
# let's just look at Ames (Iowa State) and Iowa City (Iowa) since the other 
# data sets are really limited.  

names = ['USC00130200', 'USC00134101']

# the "GHCNDStation" class is used to download data to the binary files, 
# but other classes exist to store and manipulate the data (generating a
# timeseries, removing missing values, plotting, etc.)

from pyhspf.preprocessing.ncdcstations import EvapStation

evapstations = []
for n in names:
    with open('{}/{}'.format(output, n), 'rb') as f: ghcnd = pickle.load(f)

    # make the EvapStation

    evapstation = EvapStation()

    # add the GHCND station metadata to the EvapStation

    evapstation.add_ghcnd_data(ghcnd)

    # add the EvapStation to the dictionary

    evapstations.append(evapstation)

# now we can get the time series and make a plot of the data (or whatever you 
# need to do); note there appear to be a few errors in the data set for Ames.

from matplotlib import pyplot

fig = pyplot.figure()
sub = fig.add_subplot(111)

t = ('GHCND {} data for bounding box '.format(var) + 
     '({}, {}) to ({}, {})'.format(*bbox))
sub.set_title(t)
sub.set_xlabel('Time')
sub.set_ylabel('Daily Evaporation')
sub.set_ylim([0, 30])

# times for a time series plot

times = [start + i * datetime.timedelta(days = 1) 
         for i in range((end-start).days)]

text = ''
for s in evapstations:
    total_evaporation = s.get_evaporation(start, end)
    avg_evap = total_evaporation / (end-start).days * 365.25
    values = s.make_timeseries(start, end)
    sub.plot(times, values, label = s.name)
    i = s.station, s.name, avg_evap
    text+='\nStation {}, {}\nAnnual average evaporation = {:.0f} mm'.format(*i)

sub.text(0.99, 1., text, ha = 'right', va = 'top', 
         transform = sub.transAxes, size = 8)

leg = sub.legend(loc = 'upper left', prop = {'size': 8})

pyplot.show()
