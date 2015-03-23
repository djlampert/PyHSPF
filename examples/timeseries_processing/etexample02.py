# etexample02.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 03/15/2015
#
# this example shows how to use the ETCalculator class to compute hourly 
# reference evapotranspiration from the other time series after using the
# ClimateProcessor class to extract and aggregate the climate data from the
# World Wide Web. the first part is really the same as the daily example, 
# with the differences coming at the end. the script consists of six parts: 
#
#    1. download and aggregate climate data
#    2. get pan evaporation data
#    3. get areal-weighted average latitude, longitude, and elevation
#    4. calculate daily reference evapotranspiration
#    5. calculate hourly reference evapotranspiration
#    6. compare hourly and daily over a short time period
#

import os, datetime, pickle

from pyhspf.preprocessing import ClimateProcessor, ETCalculator
from shapefile            import Reader

# output directory for data files

output = 'HSPF_data'

if not os.path.isdir(output): os.mkdir(output)

# start and end dates for data download

start = datetime.datetime(1988, 6, 1)
end   = datetime.datetime(1988, 7, 1)

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
# series has to be aggregated to an average daily value (use a different
# variable for daily and hourly)

dsolar = [sum(solar[i:i+24]) / 24 for i in range(0, 24 * (end-start).days, 24)]

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

        # remove datasets with no observations during the requested period

        observations = [v for v in data if v is not None]

        if len(observations) > 0:

            # add the data to the dictionary

            evaporation[v['name']] = data

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
calculator.add_timeseries('solar',    'daily', start, dsolar)

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

# since we don't have hourly data for temperature, wind, or dew point, some
# assumptions are needed (if anyone can suggest hourly databases for these
# parameters that would be pretty awesome BTW); the ETCalculator has a public
# method to interpolate temperatures to hourly values assuming the minimum
# temperature occurs at 6 am and the max at 4 pm and then interpolating using
# sin function for each day

hourlytemps = calculator.interpolate_temperatures(start, end)

# for wind speed and dewpoint, just assume the values are constant throughout
# the day

hdewt = [v for v in dewt for i in range(24)]
hwind = [v for v in wind for i in range(24)]

# now add the hourly time series to the calculator

calculator.add_timeseries('temperature', 'hourly', start, hourlytemps)
calculator.add_timeseries('dewpoint',    'hourly', start, hdewt)
calculator.add_timeseries('wind',        'hourly', start, hwind)

# the solar radiation data from NSRDB are already hourly

calculator.add_timeseries('solar', 'hourly', start, solar)

# calculate the reference evapotranspiration (RET) time series from the hourly 
# and daily Penman-Monteith Equation 

calculator.penman_hourly(start, end)
calculator.penman_daily(start, end)

# create pointers to the time series

RET  = calculator.hourly['RET']
dRET = calculator.daily['RET']

# aggregate the hourly back to daily

hRET = [sum(RET[i:i+24]) for i in range(0, len(RET), 24)]

# plot up the results (note that there are no observations over the winter)

from matplotlib import pyplot, dates, ticker

fig = pyplot.figure(figsize = (8,10))

# make plots for 1. evaporation (3x) 2. temperature (2x) 3. solar 4. wind

subs = []

subs.append(pyplot.subplot2grid((8,1), (0,0), rowspan = 2))
subs.append(pyplot.subplot2grid((8,1), (2,0), rowspan = 2, sharex = subs[-1]))
subs.append(pyplot.subplot2grid((8,1), (4,0), rowspan = 2, sharex = subs[-1]))
subs.append(pyplot.subplot2grid((8,1), (6,0), sharex = subs[-1]))
subs.append(pyplot.subplot2grid((8,1), (7,0), sharex = subs[-1]))

# make a list of hourly and daily times

htimes = [start + i * datetime.timedelta(hours = 1) 
          for i in range((end-start).days * 24)]
dtimes = [start + i * datetime.timedelta(days = 1) 
          for i in range((end-start).days)]

# plot the hourly and daily model ET

subs[0].plot_date(dtimes, hRET, fmt = '-', lw = 1, color = 'green', 
                  label = 'Hourly Penman-Monteith')
subs[0].plot_date(dtimes, dRET, fmt = '-', lw = 1, color = 'orange', 
                  label = 'Daily Penman-Monteith')
subs[1].plot_date(htimes, RET, fmt = '-', lw = 2, color = 'green', 
                  label = 'Hourly Penman-Monteith')

# do linear regressions on the RET vs pan evaporation data with scipy

from scipy.stats import linregress

# make a label for the series

l = ''

# plot the pan evaporation for comparison

colors = ('green', 'brown', 'blue', 'red') 
for k,c in zip(evaporation, colors):

    evap = evaporation[k]

    # the pan evaporation data are "backward-looking;" i.e., the value
    # for June 2 represents evaporation between midnight June 1 and
    # midnight June 2; whereas the ETCalculator uses data that are
    # "forward-looking;" i.e., tmin, tmax, dewpoint, wind speed for 
    # June 2 represent values between midnight June 2 and midnight
    # June 3; so the data should be shifted for comparison. since the time
    # step is hourly, the data should be disaggregated "forward." these lines 
    # will disaggregate the daily Penman-Monteith and the observed pan 
    # evaporation for comparison. 

    subs[0].plot_date(dtimes[:-1], evap[1:], fmt = 's', 
                      markerfacecolor = 'None', markeredgecolor = c, 
                      markersize = 6, label = k)

    # compare the model data (have to shift the data to compare)
    # get the linear regression

    m, b, r, p, stderr = linregress(hRET[:-1], evap[1:])

    # add a label with the correlation and pan coefficient

    its = k[:20] + ':', m, b, r**2
    l += '{:<20s}\ny = {:.2f}x + {:.2f}; r\u00B2 = {:.2f}\n'.format(*its)

# disaggregate the model and daily data for plotting

dhRET = [e / 24 for e in dRET for i in range(24)]
tmin  = [t      for t in tmin for i in range(24)]
tmax  = [t      for t in tmax for i in range(24)]
dewt  = [t      for t in dewt for i in range(24)]
wind  = [w      for w in wind for i in range(24)]

# add the disaggregated daily model ET

subs[1].plot_date(htimes, dhRET, fmt = '--', lw = 2, color = 'orange', 
                  label = 'Daily Penman-Monteith')

# add the regression info

t = subs[0].text(0.01,0.98, l, ha = 'left', va = 'top', fontsize = 9,
                 transform = subs[0].transAxes)

# add the regression info

subs[-1].xaxis.set_major_locator(dates.DayLocator((1, 7, 15, 22, 30)))
subs[-1].xaxis.set_major_formatter(dates.DateFormatter('%b-%d-%y'))

subs[0].set_ylim((0, 20))
subs[1].set_ylim((0, 1))
subs[0].set_ylabel('Daily Evaporation\n(mm)', fontsize = 11)
subs[1].set_ylabel('Hourly Evaporation\n(mm)', fontsize = 11)
subs[0].legend(fontsize = 8)
subs[1].legend(fontsize = 8)

subs[2].plot_date(htimes, hourlytemps, fmt = '-', color = 'orange', lw = 1, 
                  label = 'hourly') 
subs[2].plot_date(htimes, tmin, fmt = '-', color = 'blue', label = 'tmin', 
                  lw = 0.3)
subs[2].plot_date(htimes, tmax, fmt = '-', color = 'red', label = 'tmax', 
                  lw = 0.3)
subs[2].plot_date(htimes, dewt, fmt = '-', color = 'green', lw = 0.5,
                  label = 'dewpoint')
subs[2].set_ylabel('Temperature\n(\u00B0C)', fontsize = 11)
subs[2].legend(fontsize = 8)

subs[3].plot_date(htimes, solar, fmt = '-', color = 'orange')
subs[3].set_ylabel('Solar Radiation\n(W/m\u00B2)', fontsize = 11)

subs[4].plot_date(htimes, wind, fmt = '-', color = 'purple')
subs[4].set_ylabel('Wind\n(m/s)', fontsize = 11)

for t in (subs[0].xaxis.get_ticklabels() +
          subs[1].xaxis.get_ticklabels() +
          subs[2].xaxis.get_ticklabels() +
          subs[3].xaxis.get_ticklabels()): 
    t.set_visible(False)

for t in (subs[0].yaxis.get_ticklabels() +
          subs[1].yaxis.get_ticklabels() +
          subs[2].yaxis.get_ticklabels() +
          subs[3].yaxis.get_ticklabels()): 
    t.set_fontsize(10)

for sub in subs[-2:]: sub.yaxis.set_major_locator(ticker.MaxNLocator(6))

filename = '{}/hourly_penman_monteith'.format(output)

pyplot.suptitle('Penman-Monteith Calculation')
pyplot.subplots_adjust(hspace = 0.3, top = 0.95)
pyplot.savefig(filename)
pyplot.show()
