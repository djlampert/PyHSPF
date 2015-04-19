# precipexample.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the preprocessing tools to download hourly 
# precipitation data (DSI-3240) from the NCDC online database.

from pyhspf.preprocessing import climateutils

import os, pickle, datetime

# bounding box of interest

bbox = -94, 41.3, -91.3, 42.5 

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)
dates = start, end

# find all the hourly precipitation data from NCDC Prec3240 dataset in the box

stations = climateutils.find_precip3240(bbox, dates = dates, verbose = True)

# download the data to this "output" location

output = 'hourlyprecip'
if not os.path.isdir(output): os.mkdir(output)

# if you are using windows, the path to 7zip is needed to decompress
# the files (the default keyword is C:/Program Files/7-zip/7z.exe)

path_to_7z = r'C:/Program Files/7-Zip/7z.exe'

# download/import the data for each station; the data on the web are grouped
# by state and by year, so the same files are used for each station

for station in stations:

    # the "clean" flag deletes the source files--but the same source files
    # are needed for all these stations because they are distributed by
    # state--so removing them means they would needed to be downloaded over
    # and over again

    if not os.path.isfile('{}/{}'.format(output, station.coop)):
        
        station.download_data(output, start, end, path_to_7z = path_to_7z,
                              clean = False, plot = True)

    # many of the stations contain no data or missing data; the data are 
    # plotted and show the percent missing across the period with the output

# the statements below show how to retrieve data using station coop numbers

coops = ['132203',  # coop number for Des Moines Airport (no missing data)
         '135295',  # coop number for Maxwell, IA (12.5% missing)
         ]

# let's pull up the data for these stations and plot it

from matplotlib import pyplot, dates, ticker

for coop in coops:

    # unpickle the Precip3240 object

    with open('{}/{}'.format(output, coop), 'rb') as f: station = pickle.load(f)

    # here are a few public methods/attributes for the data in the file

    print('')
    print('station:        ', station.desc)      # description
    print('coop number:    ', station.coop)      # coop number
    print('wban number:    ', station.wban)      # weather bureau army navy no.
    print('latitude:       ', station.latitude)  # latitude
    print('longitude:      ', station.longitude) # longitude
    print('elevation (ft): ', station.elevation) # elevation (ft)

    # let's focus on the period between 2001 and 2005

    s = datetime.datetime(2001, 1, 1)
    e = datetime.datetime(2005, 1, 1)

    # get the percent missing data 

    pct = round(station.pct_missing(start = s, end = e), 1)
    print('percent missing:', pct)

    # get the total/annual average precipitation between 2001 and 2005
    
    t = station.total_precipitation(start = s, end = e)

    print('total (in):     ', round(t, 1))
    print('avg (in/yr):    ', round(t / (e - s).days * 365.25, 1))

    # get the hourly timeseries data

    prec = station.make_timeseries(start = s, end = e, tstep = 'hourly')

    # make the times

    times = [s + i * datetime.timedelta(hours = 1) 
             for i in range((end - start).days * 24)]

    # double check the data

    t = sum([p for p in prec if p is not None])

    print('total (in):     ', round(t, 1))
    print('avg (in/yr):    ', round(t / (e - s).days * 365.25, 1))
    print('')

    # plot the data to a file

    station.plot(start = s, end = e, output = '{}_data'.format(station.coop))
    station.plot(output = '{}_all'.format(station.coop))

