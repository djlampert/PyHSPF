# ghcndexample.py
#
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the preprocessing tools to download GHCND climate data.
#
# last updated: 02/05/2015

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
# if the site changes this can be updated (and this utility won't work)

# let's find the stations with evaporation data and make a list of 
# GHCNDStation instances to use to download the data. the GHCND variable 
# 'EVAP' is pan evaporation--look at the website if you are interested in 
# other variables. if the "var" keyword isn't used, it will download all
# data (as of writing this was max and min temp, snowfall, snowdepth, 
# evaporation, and wind speed).

var = 'EVAP'

# the "find_ghcnd" method will return a list of stations in the bounding box 
# and time span with "EVAP" data

stations = climateutils.find_ghcnd(bbox, var = var, dates = (start, end))
#                                   verbose = True)

# download the data to "output" location (here it's the current directory)
# each station will be saved as a binary file with its unique GHCND ID

output = os.getcwd()

for s in stations: s.download_data(output, start = start, end = end)

# there should be 4 files in the bounding box (you wouldn't know this in 
# advance); let's just look at Ames (Iowa State) and Iowa City (Iowa) since 
# the other two data sets are really limited. 

names = ['USC00130200', 'USC00134101']

# the "GHCNDStation" class is used to download data to the binary files,
# and it has some public methods available to store and manipulate the data 
# (generating timeseries, removing missing values, get the total, etc.)
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
for n in names:

    # open up the file and get the "GHCNDStation" instance we downloaded

    with open('{}/{}'.format(output, n), 'rb') as f: ghcnd = pickle.load(f)

    # get the total evaporation between "start" and "end" and convert to average

    total_evaporation = ghcnd.get_total('evaporation', start = start, end = end)
    avg_evap = total_evaporation / (end-start).days * 365.25

    # get all the values as a list between start and end

    values = ghcnd.make_timeseries('evaporation', start, end)

    # add to the plot

    sub.plot(times, values, label = ghcnd.name)
    i = ghcnd.station, ghcnd.name, avg_evap
    text+='\nStation {}, {}\nAnnual average evaporation = {:.0f} mm'.format(*i)

# add the label to the plot

sub.text(0.99, 1., text, ha = 'right', va = 'top',
         transform = sub.transAxes, size = 8)

leg = sub.legend(loc = 'upper left', prop = {'size': 8})
pyplot.show()
