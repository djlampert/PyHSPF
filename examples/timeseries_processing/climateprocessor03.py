# climateprocessor03.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 02/21/2015
#
# illustrates how to use the ClimateProcessor class to aggregate climate time 
# series (using the same data as the previous example to minimize discussion).

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

# start and end dates (just look at a few months to highlight the concept)

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(1990, 1, 2)

# make a time series

delta = datetime.timedelta(days = 1)
times = [start + i * delta for i in range((end-start).days)]

# path to 7zip if using windows

processor.path_to_7z = r'C:/Program Files/7-Zip/7z.exe'

# download the data; this step will be skipped if this has already been done
# (from the last example) but is necessary to set the metadata

processor.download(bbox, start, end, output, datasets = ['GHCND'])

# let's use the processor to aggregate the GHCND data together, including 
# tmin, tmax, snowdepth, and snowfall. The GHNCD extraction tool also has 
# wind and pan evaporation, but the wind data are sparse and pan evaporation
# isn't needed as an input to HSPF so there is not much reason to aggregate 
# those data together.

tmax      = processor.aggregate('GHCND', 'tmax',      start, end)
tmin      = processor.aggregate('GHCND', 'tmin',      start, end)
snowdepth = processor.aggregate('GHCND', 'snowdepth', start, end)
snowfall  = processor.aggregate('GHCND', 'snowfall',  start, end)

# now these time series can be saved for later consistent with the structure
# used by PyHSPF's HSPFModel class (start date, time step in minutes, data) 
# this way the data are easy to access later

for n, dataset in zip(('tmax', 'tmin', 'snowdepth', 'snowfall'), 
                      (tmax, tmin, snowdepth, snowfall)):

    name = '{}/GHCND_aggregated_{}'.format(output, n)
    ts = start, 1440, dataset

    # dump it in a pickled file to use later

    with open(name, 'wb') as f: pickle.dump(ts, f)

# plot up the results to visualize

from matplotlib import pyplot, dates

# plot up three subplots

fig, subs = pyplot.subplots(3,1,figsize = (8,10), sharex = True)

fig.suptitle('Aggregated GHCND Timeseries')
subs[0].plot_date(times, tmax, label = 'tmax', fmt = 'r-', lw = 1)
subs[0].plot_date(times, tmin, label = 'tmin', fmt = 'b-', lw = 1)

subs[1].fill_between(times, 0, snowdepth, color = 'gray', alpha = 0.5)

subs[2].plot_date(times, snowfall, fmt = '-', lw = 1, color = 'purple')

# make a band to show the range between the min and max values

subs[0].set_ylabel('Temperature (C)', fontsize = 12)
subs[0].legend(fontsize = 10)
subs[1].set_ylabel('Snowdepth (cm)', fontsize = 12)
subs[2].set_xlabel('Time', fontsize = 12)
subs[2].set_ylabel('Snowfall (cm)', fontsize = 12)
subs[2].xaxis.set_major_locator(dates.YearLocator())
subs[2].xaxis.set_major_formatter(dates.DateFormatter('%y'))

pyplot.tight_layout()
pyplot.subplots_adjust(top = 0.95)
pyplot.savefig('aggregated GHCND')
pyplot.show()
 
