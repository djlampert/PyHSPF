# example15.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to use the preprocessing tools to download climate data
# from the Global Surface Summary of the Day (GSOD) database.

from pyhspf.preprocessing import climateutils

import os, pickle, datetime

# bounding box of interest

bbox = -94, 41.3, -91.3, 42.5 

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)
dates = start, end

# download all the GSOD data (this includes daily values of tmin, tmax, 
# wind, precipitation, and dew point)

stations = climateutils.find_gsod(bbox, dates = dates, verbose = True)

# download the data to "output" location

output = 'GSOD'
if not os.path.isdir(output): os.mkdir(output)

# go through the stations and download all the data

for station in stations: 

    destination = '{0}/{1:06d}'.format(output, station.airforce)

    if not os.path.isfile(destination):

        # this will place a file in the destination directory with the 
        # station data named after the airforce number

        station.download_data(output, start = start, end = end, plot = True)

print('')

# the following lines show how to look at the data for Des Moines Airport

for station in stations:
    n = station.airforce
    filename = '{}/{}'.format(output, n)

    # unpickle the GSOD station

    with open(filename, 'rb') as f: gsodstation = pickle.load(f)

    # some attributes of the station

    print('Airforce number:', gsodstation.airforce)
    print('WBAN number:    ', gsodstation.wban)
    print('Description:    ', gsodstation.name)
    print('Latitude:       ', gsodstation.latitude)
    print('Longitude:      ', gsodstation.longitude)
    print('Elevation (m):  ', gsodstation.elevation)
    print('Data start date:', gsodstation.start)
    print('Data end date:  ', gsodstation.end)
    print('')

    # make a plot showing the data across the whole period

    #gsodstation.plot(output = '{}'.format(gsodstation.airforce))

    # let's look at the data between 2001 and 2005

    s = datetime.datetime(2001, 1, 1)
    e = datetime.datetime(2005, 1, 1)

    # get the dewpoint timeseries data

    dews = gsodstation.make_timeseries('dewpoint', start = s, end = e)
    
    # likewise for min and max temperature, wind, and precipitation

    tmin = gsodstation.make_timeseries('tmin', start = s, end = e)
    tmax = gsodstation.make_timeseries('tmax', start = s, end = e)
    wind = gsodstation.make_timeseries('wind', start = s, end = e)
    prec = gsodstation.make_timeseries('precipitation', start = s, end = e)

    # missing values are Nones, so this is how they can be found (the following
    # removes them)

    tmin = [t for t in tmin if t is not None]
    tmax = [t for t in tmax if t is not None]
    wind = [w for w in wind if w is not None]

    annual_prec = sum([p for p in prec if p is not None]) / (e-s).days * 365.25

    print('avg dewpoint temperature: {:.1f} C'.format(sum(dews) / len(dews)))
    print('avg minimum temperature:  {:.1f} C'.format(sum(tmin) / len(tmin)))
    print('avg maximum temperature:  {:.1f} C'.format(sum(tmax) / len(tmax)))
    print('avg wind speed:           {:.1f} m/s'.format(sum(wind) / len(wind)))
    print('avg precipitation:        {:.1f} mm'.format(annual_prec))
    print('')
