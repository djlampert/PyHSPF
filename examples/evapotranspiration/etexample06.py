# etexample06.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 04/03/2015
#
# this example shows how to use the ETCalculator class to calculate hourly 
# potential evapotranspiration time series for a number of common crops using
# the Food and Agriculture Organization methods. the first part is really the 
# same as the previous examples, with the differences coming at the end. the 
# script consists of five parts: 
#
#    1. download and aggregate climate data
#    2. get areal-weighted average latitude, longitude, and elevation
#    3. calculate hourly reference evapotranspiration
#    4. calculate potential evapotranspiration for various landuse categories
#    5. aggregate hourly estimates and plot results
#

import os, datetime, pickle

from pyhspf.preprocessing import ClimateProcessor, ETCalculator
from shapefile            import Reader

# output directory for data files

output = 'HSPF_data'

if not os.path.isdir(output): os.mkdir(output)

# start and end dates for data download

start = datetime.datetime(2000, 1, 1)
end   = datetime.datetime(2005, 1, 1)

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

# let's get the daily tmin, tmax, dewpoint, wind speed and solar

tmax  = processor.aggregate('GSOD',  'tmax',     start, end)
tmin  = processor.aggregate('GSOD',  'tmin',     start, end)
dewt  = processor.aggregate('GSOD',  'dewpoint', start, end)
wind  = processor.aggregate('GSOD',  'wind',     start, end)
solar = processor.aggregate('NSRDB', 'metstat',  start, end)

# use the ETCalculator to estimate the evapotranspiration time series

calculator = ETCalculator()

# some of the parameters in the Penman-Monteith Equation depend on the 
# geographic location so get the average longitude, latitude, and elevation

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

# use the daily tmin and tmax time series to the calculator to get hourly temps

calculator.add_timeseries('tmin', 'daily', start, tmin)
calculator.add_timeseries('tmax', 'daily', start, tmax)

# calculate the hourly temperature time series

hourlytemps = calculator.interpolate_temperatures(start, end)

# assume the values for wind speed and dewpoint are constant throughout the day

hdewt = [v for v in dewt for i in range(24)]
hwind = [v for v in wind for i in range(24)]

# add the hourly time series to the calculator

calculator.add_timeseries('temperature', 'hourly', start, hourlytemps)
calculator.add_timeseries('dewpoint',    'hourly', start, hdewt)
calculator.add_timeseries('wind',        'hourly', start, hwind)
calculator.add_timeseries('solar',       'hourly', start, solar)

# crop growth dates/times and coefficients

crop      = 'corn'
plant     = datetime.datetime(2000, 4, 15)
emergence = 30
growth    = 50
full      = 60
late      = 40
Ki        = 0.30
Km        = 1.15
Kl        = 0.40

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# crop growth dates/times and coefficients

crop      = 'soybeans'
plant     = datetime.datetime(2000, 5, 15)
emergence = 20
growth    = 30
full      = 60
late      = 30
Ki        = 0.40
Km        = 1.15
Kl        = 0.55

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# crop growth dates/times and coefficients

crop      = 'small grains'
plant     = datetime.datetime(2000, 4, 15)
emergence = 20
growth    = 30
full      = 60
late      = 40
Ki        = 0.30
Km        = 1.15
Kl        = 0.40

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# crop growth dates/times and coefficients

crop      = 'alfalfa'
plant     = datetime.datetime(2000, 5, 15)
emergence = 10
growth    = 10
full      = 120
late      = 10
Ki        = 0.30
Km        = 0.95
Kl        = 0.90

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# fallow land--just use 0.3 all the time 

crop      = 'fallow'
plant     = datetime.datetime(2000, 3, 1)
emergence = 10
growth    = 10
full      = 240
late      = 10
Ki        = 0.30
Km        = 0.30
Kl        = 0.30

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# crop growth dates/times and coefficients for FAO rotated grazing

crop      = 'pasture'
plant     = datetime.datetime(2000, 3, 1)
emergence = 10
growth    = 10
full      = 240
late      = 10
Ki        = 0.30
Km        = 0.85
Kl        = 0.30

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# crop growth dates/times and coefficients (FAO reed swamp)

crop      = 'wetlands'
plant     = datetime.datetime(2000, 3, 1)
emergence = 10
growth    = 10
full      = 240
late      = 10
Ki        = 1.0
Km        = 1.2
Kl        = 1.0

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# crop growth dates/times and coefficients (for others just use RET)

crop      = 'others'
plant     = datetime.datetime(2000, 3, 1)
emergence = 10
growth    = 10
full      = 240
late      = 10
Ki        = 1.0
Km        = 1.0
Kl        = 1.0

# add the information to the etcalculator and calculate the PET time series

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)
calculator.hourly_PET(crop, start, end)

# plot the results

from matplotlib import pyplot, dates, ticker

fig = pyplot.figure()
sub = fig.add_subplot(111)

# make a time series

times = [start + i * datetime.timedelta(days = 1)
         for i in range((end-start).days)]

# all the landuse categories

landuse = ('corn', 'soybeans', 'small grains', 'alfalfa', 'fallow',
           'pasture', 'wetlands', 'others')
colors  = ('yellow',  'green',        'brown',    'lime',   'gray', 
           'orange',      'blue', 'black')

# iterate through and plot the results

for crop, color in zip(landuse, colors):

    # get the PET time series
    
    start, PET = calculator.hourlyPETs[crop]

    # aggregate the hourly to daily values

    PET = [sum(PET[i:i+24]) for i in range(0, len(PET), 24)]

    sub.plot_date(times, PET, fmt = '-', color = color, label = crop)

sub.set_title('Potential Evapotranspiration Time Series')
sub.set_ylabel('Evapotranspiration\n(mm)')
sub.yaxis.set_major_locator(ticker.MaxNLocator(8))
sub.xaxis.set_major_locator(dates.MonthLocator((6,12)))
sub.xaxis.set_major_formatter(dates.DateFormatter('%b-%y'))
sub.yaxis.set_major_locator(ticker.MaxNLocator(5))

sub.legend()
pyplot.tight_layout()
pyplot.savefig('potential_evapotranspiration')
pyplot.show()
