# etexample03.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 03/22/2015
#
# this example shows how to use the ETCalculator class to compute hourly 
# reference evapotranspiration from the other time series after using the
# ClimateProcessor class to extract and aggregate the climate data from the
# World Wide Web. the first part is really the same as the previous example, 
# with the differences coming at the end. the script consists of five parts: 
#
#    1. download and aggregate climate data
#    2. get pan evaporation data
#    3. get areal-weighted average latitude, longitude, and elevation
#    4. calculate hourly reference evapotranspiration
#    5. aggregate hourly estimates and compare with pan evaporation observations
#

import os, datetime, pickle

from pyhspf.preprocessing import ClimateProcessor, ETCalculator
from shapefile            import Reader

# output directory for data files

output = 'HSPF_data'

if not os.path.isdir(output): os.mkdir(output)

# start and end dates for data download

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

# aggregate the hourly solar data to daily for plotting

dsolar = [sum(solar[i:i+24]) / 24 for i in range(0, 24 * (end-start).days, 24)]

# the ETCalculator class uses the Penman-Monteith Equation to estimate 
# evapotranspiration time series; make an instance to use

calculator = ETCalculator()

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

# add the daily time series to the calculator for plotting

calculator.add_timeseries('tmin',     'daily', start, tmin)
calculator.add_timeseries('tmax',     'daily', start, tmax)
calculator.add_timeseries('dewpoint', 'daily', start, dewt)
calculator.add_timeseries('wind',     'daily', start, wind)
calculator.add_timeseries('solar',    'daily', start, dsolar)

# calculate the hourly temperature time series

hourlytemps = calculator.interpolate_temperatures(start, end)

# assume the values for wind speed and dewpoint are constant throughout the day

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

# save the time series for later (i.e., to add to an HSPF Model)

RET = [e for e in calculator.hourly['RET'][1]]

data = start, 60, RET
filename = '{}/hourlyRET'.format(output)
with open(filename, 'wb') as f: pickle.dump(data, f)

# aggregate the hourly to daily and add it to the calculator for plotting

dRET = [sum(RET[i:i+24]) for i in range(0, len(RET), 24)]

calculator.add_timeseries('RET', 'daily', start, dRET)

# parse the GHCND data for pan evaporation observations and store the file
# paths (the ETCalculator will automatically extract the data from the file)

evaporations = []

for k, v in processor.metadata.ghcndstations.items():

    if v['evap'] > 0: evaporations.append(k)

# the ETCalculator has a few public plotting methods including a plot similar
# to the first example (daily ET) 

filename = '{}/referenceET'.format(output)

# the plotET method has a few optional keyword arguments including a dictionary
# of GHCND stations for comparing the model with pan evaporation data, 
# start and end dates, and some other plotting parameters but using 
# hourly estimates aggregated to a daily time step

calculator.plotET(stations = evaporations, output = filename, show = True)

# the "dayofyear" plot converts long time series to the water year 
# (Oct 1, Year -- Sept 30, Year + 1). these plots provide a useful way to 
# examine long-term trends in hydrology of the watershed

filename = '{}/dayofyearET'.format(output)

# note that there appears to be an error in the Upper Marlboro dataset in 2000

calculator.plotdayofyear(stations = evaporations, output = filename, 
                         show = True)
