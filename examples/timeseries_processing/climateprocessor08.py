# climateprocessor08.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 03/07/2015
#
# this example illustrates how to use the ClimateProcessor class to aggregate 
# timeseries data using an inverse distance weighted average (IDWA). makes use 
# of the "subbasin_catchments" shapefile in this directory that was generated 
# using the NHDPlusExtractor and HUC8Delineator classes shown elsewhere. The 
# result of the code is the same as the last example, but this shows how to 
# use the ClimateProcessor's public methods rather than a manual aggregation.

import os, datetime, pickle

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

# the ClimateProcessor's aggregate method can be used with inverse-distance
# weighted average (IDWA) to interpolate between the stations at a given point
# using the "method," "latitude," and "longitude" keyword arguments. the
# result is the same as the previous example. as before, the subbasin_catchments
# shapefile will be used that contains the centroid for each aggregation.

sf = Reader(filename)

# index of the comid, latitude, and longitude records

comid_index = [f[0] for f in sf.fields].index('ComID') - 1
lon_index   = [f[0] for f in sf.fields].index('CenX')  - 1
lat_index   = [f[0] for f in sf.fields].index('CenY')  - 1

# iterate through the shapefile records and aggregate the timeseries

if 1==2:
#for i in range(len(sf.records())):

    record = sf.record(i)
    comid  = record[comid_index]
    lon    = record[lon_index]
    lat    = record[lat_index]

    i = comid, lon, lat
    print('aggregating timeseries for comid {} at {}, {}\n'.format(*i))

    precipitation = processor.aggregate('precip3240', 'precip', start, end,
                                        method = 'IDWA', longitude = lon,
                                        latitude = lat)

    mean = sum(precipitation) / (end - start).days * 365.25

    print('aggregated annual average precipitation: {:.1f} in\n'.format(mean))

    # dump the result in PyHSPF timeseries format into a pickle file for later

    ts = start, 60, precipitation

    # use the unique common identifiers for the files

    p = '{}/{}'.format(directory, comid)
    with open(p, 'wb') as f: pickle.dump(ts, f)

# now let's do one simple aggregation and compare the results

#simple = processor.aggregate('precip3240', 'precip', start, end)

# dump it

#ts = start, 60, simple
#with open('{}/simple'.format(directory), 'wb') as f: pickle.dump(ts, f)

i = filename, directory
print('done aggregating all timeseries in {}, results in {}\n'.format(*i))

# plot up the results

from matplotlib import pyplot, dates, ticker

fig, subs = pyplot.subplots(3,2, figsize = (10,8))

fig.suptitle('Comparison of Precipitation Aggregation Techniques', 
             fontsize = 14)

# let's compare results from several stations including BWI (COOP 180465), 
# Beltsville (COOP 180700), Aberdeen (COOP  180015), the hunting creek 
# watershed (ComID 11908470), and the unweighted average for some storm events

storms = [(datetime.datetime(1989, 5, 1, 8),
           datetime.datetime(1989, 5, 2, 12),
           subs[0,0]),
          (datetime.datetime(1989, 6, 6, 18),
           datetime.datetime(1989, 6, 7, 15),
           subs[1,0]),
          (datetime.datetime(1989, 9, 25, 12),
           datetime.datetime(1989, 9, 26, 14),
           subs[2,0]),
          (datetime.datetime(1989, 10, 1, 12),
           datetime.datetime(1989, 10, 2, 16),
           subs[0,1]),
          (datetime.datetime(1990, 5, 4, 12),
           datetime.datetime(1990, 5, 5, 4),
           subs[1,1]),
          (datetime.datetime(1990, 6, 14, 22),
           datetime.datetime(1990, 6, 15, 10),
           subs[2,1]),
          ]

for s, e, sub in storms:

    # get the data for BWI Airport

    p = '{}/precip3240/180465'.format(output)

    with open(p, 'rb') as f: station = pickle.load(f)

    bwi = station.make_timeseries('precip', s, e)

    # get the data for Beltsville

    p = '{}/precip3240/180700'.format(output)

    with open(p, 'rb') as f: station = pickle.load(f)

    belts = station.make_timeseries('precip', s, e)

    # get the data for Aberdeen

    p = '{}/precip3240/180015'.format(output)

    with open(p, 'rb') as f: station = pickle.load(f)

    aber = station.make_timeseries('precip', s, e)

    # get the data for Millers

    p = '{}/precip3240/185934'.format(output)

    with open(p, 'rb') as f: station = pickle.load(f)

    mill = station.make_timeseries('precip', s, e)

    # get the simple aggregation

    p = '{}/simple'.format(directory)

    with open(p, 'rb') as f: start, tstep, data = pickle.load(f)

    # find the indices for the start and end dates (1440 min/day, tstep in min)

    i = (s-start).days * 24 + (s-start).seconds // 3600
    j = (e-start).days * 24 + (e-start).seconds // 3600

    simple = data[i:j]

    # find the IDWA aggregation

    comid = 11908470
    p = '{}/{}'.format(directory, comid)

    with open(p, 'rb') as f: start, tstep, data = pickle.load(f)

    # find the indices for the start and end dates

    i = (s-start).days * 24 + (s-start).seconds // 3600
    j = (e-start).days * 24 + (e-start).seconds // 3600

    IDWA = data[i:j]

    # make a list of the times

    delta = datetime.timedelta(hours = 1)
    times = [s + i * delta for i in range(j-i)]

    sub.plot_date(times, IDWA, fmt = '--', color =  'black', lw = 2, 
                  label = 'inverse distance weighting')
    sub.plot_date(times, simple, fmt = '-', color = 'red', lw = 2,
                  label = 'simple aggregation')
    sub.plot_date(times, bwi, fmt = 's', markeredgecolor = 'green', 
                  markersize = 6, alpha = 0.3, color = 'green', 
                  markeredgewidth = 2, label = 'BWI Airport')
    sub.plot_date(times, belts, fmt = 'o', markeredgecolor = 'purple', 
                  markersize = 6, alpha = 0.5, color = 'purple',
                  markeredgewidth = 2, label = 'Beltsville')
    sub.plot_date(times, aber, fmt = 'x', markeredgecolor = 'orange', 
                  markersize = 6, color = 'orange',
                  markeredgewidth = 2, label = 'Aberdeen')
    sub.plot_date(times, mill, fmt = 'd', markeredgecolor = 'brown', 
                  markersize = 6, alpha = 0.5, color = 'brown',
                  markeredgewidth = 2, label = 'Millers')

    sub.xaxis.set_major_locator(dates.DayLocator())
    sub.xaxis.set_minor_locator(dates.HourLocator((0, 4, 8, 12, 16, 20)))
    sub.xaxis.set_major_formatter(dates.DateFormatter('\n%b-%d-%Y'))
    sub.xaxis.set_minor_formatter(dates.DateFormatter('%H:00'))
    sub.yaxis.set_major_locator(ticker.MultipleLocator(.1))
    sub.yaxis.set_major_formatter(ticker.FormatStrFormatter('%.1f'))

    for t in (sub.xaxis.get_ticklabels() +
              sub.xaxis.get_minorticklabels() +
              sub.yaxis.get_ticklabels()): 
        t.set_fontsize(11)

    sub.grid(which = 'minor')
    sub.grid(which = 'major')
    sub.set_ylabel('Hourly Precipitation (in)', fontsize = 11)

pyplot.legend(fontsize = 10, bbox_to_anchor = (-0.1, -0.24), loc = 9, ncol = 3)

pyplot.subplots_adjust(bottom = 0.14, top = 0.93, hspace = 0.28, wspace = 0.22)
pyplot.savefig('IDWA_aggregation')
pyplot.show()

