# climateprocessor07.py
# 
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 02/28/2015
#
# illustrates how to use the ClimateProcessor class to aggregate raw 
# precipitation data into subbasin-specific time series using an inverse 
# distance weighted average.

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

# start and end dates (aggregate the whole 31 years)

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# path to 7zip if using windows

processor.path_to_7z = r'C:/Program Files/7-Zip/7z.exe'

# download or set the location of the data; this shows how to use the path to
# an existing shapefile to define the data download area

processor.download_shapefile('subbasin_catchments', start, end, output, 
                             datasets = ['precip3240'], space = 0.5)


precip = processor.aggregate('precip3240', 'precip', start, end)

#exit()
# the time series can be saved for later consistent with the PyHSPF format

name = '{}/precip3240_aggregated_precip'.format(output)
ts = start, 60, precip

# dump it in a pickled file to use later

with open(name, 'wb') as f: pickle.dump(ts, f)

# change the start and end dates for plotting up some hourly results
# (just look at a few months to highlight the concept)

start = datetime.datetime(2001, 5, 15)
end   = datetime.datetime(2001, 6, 20)

# make a time series -- note that the solar radiation data are hourly

delta = datetime.timedelta(hours = 1)
times = [start + i * delta for i in range((end-start).days * 24)]

# let's use the processor to aggregate the NSRDB data together for the 
# different data sources in the NSRDB

precip = processor.aggregate('precip3240', 'precip', start, end)

# plot up the results to help visualize the data and make sure it's right

from matplotlib import pyplot, dates

fig, sub = pyplot.subplots(1,1)

fig.suptitle('Aggregated Precipitation Timeseries')
sub.fill_between(times, 0, precip, color = 'blue', alpha = 0.5)

sub.set_ylabel('Precipitation (in)', fontsize = 12)
sub.xaxis.set_major_locator(dates.DayLocator((1, 10, 20)))
sub.xaxis.set_major_formatter(dates.DateFormatter('%b-%d-%Y'))

pyplot.tight_layout()
pyplot.subplots_adjust(top = 0.95)
pyplot.savefig('aggregated precipitation')
pyplot.show()
 
