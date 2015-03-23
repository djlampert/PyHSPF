# etexample01.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 03/15/2015
#
# this example shows how to use the ETCalculator class to compute daily 
# reference evapotranspiration from the other time series after using the
# ClimateProcessor class to extract and aggregate the climate data from the
# World Wide Web. the script consists of five parts: 
#
#    1. download and aggregate climate data
#    2. get pan evaporation data
#    3. get areal-weighted average latitude, longitude, and elevation
#    4. calculate reference evapotranspiration
#    5. plot (4) vs (2) for comparison
#

import os, datetime, pickle

from pyhspf.preprocessing import ClimateProcessor, ETCalculator
from shapefile            import Reader

# output directory for data files

output = 'HSPF_data'

if not os.path.isdir(output): os.mkdir(output)

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2010, 1, 1)

# use the "subbasin_catchments" shapefile to define the data processing area

filename = 'subbasin_catchments'

if not os.path.isfile(filename + '.shp'):
    print('error: file {} does not exist!'.format(filename))
    raise

# make an instance of the ClimateProcessor to fetch the climate data

processor = ClimateProcessor()

# the Penman-Monteith Equation requires temperature, humidity of dewpoint,
# wind speed, and solar radiation, which can be obtained from the processor

processor.download_shapefile(filename, start, end, output, space = 0.)

# let's get the daily tmin, tmax, dewpoint, and wind speed from GSOD

tmax  = processor.aggregate('GSOD', 'tmax', start, end)
tmin  = processor.aggregate('GSOD', 'tmin', start, end)
dewt  = processor.aggregate('GSOD', 'dewpoint', start, end)
wind  = processor.aggregate('GSOD', 'wind', start, end)

# let's use the hourly METSTAT data from the NSRDB for solar radiation

solar = processor.aggregate('NSRDB', 'metstat', start, end)

# since the solar data are hourly and this example uses daily ET, the time
# series has to be aggregated to an average daily value

solar = [sum(solar[i:i+24]) / 24 for i in range(0, 24 * (end-start).days, 24)]

# let's parse the GHCND data and see if there are any pan evaporation 
# observations (only available from GHCND) to compare with estimated PET 
# (there are two stations with data); let's store the data in a dictionary 
# structure with keys as names and values as the timeseries data

evaporation = {}

for k, v in processor.metadata.ghcndstations.items():

    if v['evap'] > 0:

        its = v['name'], v['evap']
        print('station {} has {} evaporation observations'.format(*its))

        # open up the file and use the GHCNDStation instance to get the data

        with open(k, 'rb') as f: s = pickle.load(f)

        data = s.make_timeseries('evaporation', start, end)

        # ignore datasets with no observations during the requested period

        observations = [v for v in data if v is not None]

        if len(observations) > 0:

            # the pan evaporation data are "backward-looking;" i.e., the value
            # for June 2 represents evaporation between midnight June 1 and
            # midnight June 2; whereas the ETCalculator uses data that are
            # "forward-looking;" i.e., tmin, tmax, dewpoint, wind speed for 
            # June 2 represent values between midnight June 2 and midnight
            # June 3; so the pan evaporation data must be shifted forward by
            # a day for comparison. 

            evaporation[v['name']] = data[1:] + [None]

print('')

# the ETCalculator class uses the Penman-Monteith Equation to estimate 
# evapotranspiration time series; let's make an instance to use

calculator = ETCalculator()

# the ETCalculator makes use of hourly and daily time series that must be
# supplied externally as:
#
#   1. time series type (temperature, dewpoint, humidity, wind, solar, etc)
#   2. time step step ("daily", "hourly", or size in minutes, e.g., 60, 1440)
#   3. start time
#   4. list of data values
#
# note these are the same formats used by the PyHSPF HSPFModel class
# so now we can add the daily timeseries from above

calculator.add_timeseries('tmin',     'daily', start, tmin)
calculator.add_timeseries('tmax',     'daily', start, tmax)
calculator.add_timeseries('dewpoint', 'daily', start, dewt)
calculator.add_timeseries('wind',     'daily', start, wind)
calculator.add_timeseries('solar',    'daily', start, solar)

# the temperature and dewpoint are assumed to be in C, wind speed in m/s, and
# solar radiation in W/m2; these are the units supplied by the other classes
# in PyHSPF already so no manipulation is needed

# some of the parameters in the Penman-Monteith Equation depend on the 
# geographic location so let's use the information in the shapefile to 
# provide the average longitude, latitude, and elevation

sf = Reader(filename)

# make a list of the fields for each shape

fields = [f[0] for f in sf.fields]

# get the area, centroid and elevation of each shape

areas = [r[fields.index('AreaSqKm') - 1] for r in sf.records()]
xs    = [r[fields.index('CenX')     - 1] for r in sf.records()]
ys    = [r[fields.index('CenY')     - 1] for r in sf.records()]
zs    = [r[fields.index('AvgElevM') - 1] for r in sf.records()]

# get the areal-weighted averages

lon  = sum([a * x for a, x in zip(areas, xs)]) / sum(areas)
lat  = sum([a * y for a, y in zip(areas, ys)]) / sum(areas)
elev = sum([a * z for a, z in zip(areas, zs)]) / sum(areas)

# add the information to the calculator

calculator.add_location(lon, lat, elev)

# it is pretty trivial to get the corresponding reference evapotranspiration 
# (RET) time series from the Daily Penman-Monteith Equation if the necessary 
# data are available by calling the public "penman_daily" method

calculator.penman_daily(start, end)

# the timeseries is stored in the timeseries dictionary 

RET = calculator.daily['RET']

# calculate the linear regression between the Penman-Monteith model and the 
# observed pan evaporation using scipy

from scipy import stats

# plot up the results (note that there are no observations over the winter)

from matplotlib import pyplot, dates, ticker

fig = pyplot.figure(figsize = (8,10))

# make plots for 1. evaporation (3x) 2. temperature (2x) 3. solar 4. wind

subs = []

subs.append(pyplot.subplot2grid((7,1), (0,0), rowspan = 3))
subs.append(pyplot.subplot2grid((7,1), (3,0), rowspan = 2, sharex = subs[-1]))
subs.append(pyplot.subplot2grid((7,1), (5,0)))
subs.append(pyplot.subplot2grid((7,1), (6,0)))

# make a list of times

times = [start + i * datetime.timedelta(days = 1) 
         for i in range((end-start).days)]

subs[0].plot_date(times, RET, fmt = '-', lw = 2, color = 'green', 
                  label = 'Penman-Monteith Equation')

# make a label for the series

l = ''

# plot the pan evaporation for comparison

colors = ('green', 'brown', 'blue', 'red') 
for k,c in zip(evaporation, colors):

    v = evaporation[k]

    # remove the Nones (first have to remove the predictions that have no
    # corresponding observation)

    model = [m for o,m in zip(v, RET) if o is not None]
    data  = [o for o,m in zip(v, RET) if o is not None]

    m, b, r, p, stderr = stats.linregress(model, data)

    subs[0].plot_date(times, v, fmt = 's', markerfacecolor = 'None', 
                      markeredgecolor = c, markersize = 4, label = k)

    # add a label with the correlation and pan coefficient

    its = k[:20] + ':', m, b, r**2
    l += '{:<20s}\ny = {:.2f}x + {:.2f}; r\u00B2 = {:.2f}\n'.format(*its)

# add the regression info

t = subs[0].text(0.01,0.98, l, ha = 'left', va = 'top', fontsize = 9,
                 transform = subs[0].transAxes)

subs[-1].xaxis.set_major_locator(dates.YearLocator(3))
subs[-1].xaxis.set_major_formatter(dates.DateFormatter('%Y'))

subs[0].set_ylim((0, 12))
subs[0].set_ylabel('Evaporation (mm)', fontsize = 11)
subs[0].legend(fontsize = 8)

subs[1].plot_date(times, tmin, fmt = '-', color = 'blue', label = 'tmin')
subs[1].plot_date(times, tmax, fmt = '-', color = 'red', label = 'tmax')
subs[1].plot_date(times, dewt, fmt = '-', color = 'green', 
                  label = 'dewpoint')
subs[1].set_ylabel('Temperature\n(\u00B0C)', fontsize = 11)
subs[1].legend(fontsize = 8)

subs[2].plot_date(times, solar, fmt = '-', color = 'orange')
subs[2].set_ylabel('Solar Radiation\n(W/m\u00B2)', fontsize = 11)

subs[3].plot_date(times, wind, fmt = '-', color = 'purple')
subs[3].set_ylabel('Wind\n(m/s)', fontsize = 11)

for t in (subs[0].xaxis.get_ticklabels() +
          subs[1].xaxis.get_ticklabels() +
          subs[2].xaxis.get_ticklabels()): 
    t.set_visible(False)

for t in (subs[0].yaxis.get_ticklabels() +
          subs[1].yaxis.get_ticklabels() +
          subs[2].yaxis.get_ticklabels() +
          subs[3].yaxis.get_ticklabels()): 
    t.set_fontsize(10)

for sub in subs[-2:]: sub.yaxis.set_major_locator(ticker.MaxNLocator(6))

filename = '{}/daily_penman_monteith'.format(output)

pyplot.suptitle('Penman-Monteith Calculation')
pyplot.subplots_adjust(hspace = 0.3, top = 0.95)
pyplot.savefig(filename)
pyplot.show()
