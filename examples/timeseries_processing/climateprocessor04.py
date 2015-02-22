# climateprocessor04.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 02/21/2015
#
# illustrates how to use the ClimateProcessor class to aggregate climate time 
# series from the GSOD database (very similar to the last example)

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

processor.download(bbox, start, end, output, datasets = ['GSOD'])

# let's use the processor to aggregate the GSOD data together, including 
# tmin, tmax, dewpoint, and wind speed. The GSOD database contains dew point
# and seems to be more complete than GHCND. It doesn't have snow or pan 
# evaporation though--this is why they are both included.

tmax     = processor.aggregate('GSOD', 'tmax',     start, end)
tmin     = processor.aggregate('GSOD', 'tmin',     start, end)
dewpoint = processor.aggregate('GSOD', 'dewpoint', start, end)
wind     = processor.aggregate('GSOD', 'wind',     start, end)

# now these time series can be saved for later consistent with the structure
# used by PyHSPF's HSPFModel class (start date, time step in minutes, data) 
# this way the data are easy to access later

for n, dataset in zip(('tmax', 'tmin', 'dewpoint', 'wind'), 
                      (tmax, tmin, dewpoint, wind)):

    name = '{}/GSOD_aggregated_{}'.format(output, n)
    ts = start, 1440, dataset

    # dump it in a pickled file to use later

    with open(name, 'wb') as f: pickle.dump(ts, f)

# plot up the results to visualize

from matplotlib import pyplot, dates

# invert the time series back

fig, subs = pyplot.subplots(2,1,figsize = (8,8), sharex = True)

fig.suptitle('Aggregated GSOD Timeseries')
subs[0].plot_date(times, tmax,     label = 'tmax', fmt = 'r-', lw = 1)
subs[0].plot_date(times, tmin,     label = 'tmin', fmt = 'b-', lw = 1)
subs[0].plot_date(times, dewpoint, label = 'dewpoint', fmt = 'g-', lw = 1)

subs[1].plot_date(times, wind, fmt = '-', color = 'purple', lw = 1)

# make a band to show the range between the min and max values

subs[1].set_xlabel('Time', fontsize = 12)
subs[0].set_ylabel('Temperature (C)', fontsize = 12)
subs[1].set_ylabel('Wind Speed (m/s)', fontsize = 12)
subs[1].xaxis.set_major_locator(dates.YearLocator())
subs[1].xaxis.set_major_formatter(dates.DateFormatter('%y'))
subs[0].legend(fontsize = 10)

pyplot.tight_layout()
pyplot.subplots_adjust(top = 0.95)
pyplot.savefig('aggregated GSOD')
pyplot.show()
 
