# example16.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the preprocessing tools to download solar radiation
# data from the National Solar Radiation Database

from pyhspf.preprocessing import climateutils

import os, pickle, datetime

# bounding box of interest

bbox = -94, 41.3, -91.3, 42.5 

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)
dates = start, end

# find all the solar radiation stations from the National Solar Radiation
# Database (NSRDB) within the bounding box

stations = climateutils.find_nsrdb(bbox, dates = dates)

# download the data to "output" location for each station

output = 'NSRDB'
if not os.path.isdir(output): os.mkdir(output)

for station in stations: 

    if not os.path.isfile('{}/{}'.format(output, station.usaf)):

        station.download_data(output, dates = (start, end))

        # the data are pickled to files in the "output" directory by their wban

# iterate through the stations and get some information

stations = os.listdir(output)

for s in stations:

    # open up the file

    with open('{}/{}'.format(output, s), 'rb') as f: station = pickle.load(f)

    # print the metadata

    print('station name:   ', station.station)
    print('elevation (m):  ', station.elevation)
    print('airforce number:', station.usaf)
    print('wban number:    ', station.wban)

    # the database has data for the METSTAT model, the SUNY model, any 
    # observed data (very scarce), and "legacy data" before 1990

    # get a daily timeseries for the METSTAT model for the year 2001; 
    # set the dataset keyword to 'suny' or 'observed' to look at others 
    # although there are very few stations with actual observed data
    
    s = datetime.datetime(2001, 1, 1)
    e = datetime.datetime(2002, 1, 1)

    # the timeseries is a list of values for each time step

    ts = station.make_timeseries(dataset = 'metstat',
                                 start = s, end = e, tstep = 'daily')

    # the units are watts per m2; so the sum really represents the total 
    # solar radiation energy in Joules per square meter per total time;
    # let's convert to kWh/m2/day average (more typical units)

    t = sum(ts) / 1000 / (e - s).days
    
    print('')
    print('average solar radiation (kWh/m2/d): {:.2f}'.format(t))
    print('')

    # make a plot of hourly data for month of June 2001 for the Des Moines AP

    if station.usaf == '725460':

        s = datetime.datetime(2001, 6, 1)
        e = datetime.datetime(2001, 7, 1)

        station.plot(start = s, end = e, tstep = 'hourly', 
                     output = '{}hourly'.format(station.usaf))

        # likewise for daily

        station.plot(start = s, end = e, tstep = 'daily',
                     output = '{}daily'.format(station.usaf))

        # and monthly as an average (let's go back to 2001 - 2005)

        s = datetime.datetime(2001, 1, 1)
        e = datetime.datetime(2005, 1, 1)

        station.plot(start = s, end = e, tstep = 'monthly',
                     output = '{}monthly'.format(station.usaf))
