# example6.py
# David Lampert
# illustrates how to use the preprocessing tools to download climate data.

from pyhspf.preprocessing.climateplots     import plot_hourlyET
from pyhspf.preprocessing.download_climate import find_ghcnd

import os, pickle, datetime

# bounding box of interest

bbox = -94, 41.3, -91.3, 42.5 

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# GHCND variable to look for data ('EVAP' is pan evaporation--look this up)

var = 'EVAP'

# url to the NCDC database for the GHCND

GHCND = 'http://www1.ncdc.noaa.gov/pub/data/ghcn/daily'

# find the stations with evaporation data and make a list of GHCNDStation 
# instances to use to download the data for the stations

stations = find_ghcnd(bbox, GHCND, var = var, dates = (start, end),
                      verbose = True)

# download the data to "output" location (here it's the current directory, ".")
# each station will be saved as a binary file with its unique GHCND ID

output = '.'

for station in stations: station.download_data(output, start = start, end = end,
                                               plot = True)

# there are 4 files in the bounding box (you wouldn't know this in advance); 
# let's just look at Ames (Iowa State) and Iowa City (Iowa) since the other 
# data sets are really limited.  

names = ['USC00130200', 'USC00134101']

# the GHCND class is used to download data, but an additional class exists to
# store it and do things like make timeseries for removing missing values, etc.

from pyhspf.preprocessing.ncdcstations import EvapStation

evapstations = []
for n in names:
    with open('{}/{}'.format(output, n), 'rb') as f: ghcnd = pickle.load(f)

    # make the EvapStation

    evapstation = EvapStation()

    # add the GHCND station metadata to the EvapStation

    evapstation.add_location(ghcnd)

    # add the GHCND station data to the EvapStation

    evapstation.add_data(ghcnd.evap)

    # add the EvapStation to the dictionary

    evapstations.append(evapstation)

# now we can plot the time series (or whatever you need the data for)
# note that there appears to be a few errors in the data set for Ames

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
