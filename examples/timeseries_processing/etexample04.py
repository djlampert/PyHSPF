# etexample04.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 04/03/2015
#
# this example shows how to use the ETCalculator class to convert daily 
# reference evapotranspiration to daily potential evapotranspiration using
# crop coefficients consistent with the Food and Agriculture Organization's
# methodology. the first part is really the same as the previous examples, 
# with the differences coming at the end. the script consists of five parts: 
#
#    1. download and aggregate climate data
#    2. get areal-weighted average latitude, longitude, and elevation
#    3. calculate daily reference evapotranspiration
#    4. convert reference evapotranspiration to potential evapotranspiration 
#    5. plot results
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

# add the daily time series to the calculator

calculator.add_timeseries('tmin',     'daily', start, tmin)
calculator.add_timeseries('tmax',     'daily', start, tmax)
calculator.add_timeseries('dewpoint', 'daily', start, dewt)
calculator.add_timeseries('wind',     'daily', start, wind)
calculator.add_timeseries('solar',    'daily', start, solar)

# calculate the reference evapotranspiration (RET) time series from the daily 
# Penman-Monteith Equation 

calculator.penman_daily(start, end)

# save the time series for later (i.e., to add to an HSPF Model)

RET = [e for e in calculator.daily['RET'][1]]

data = start, 1440, RET
filename = '{}/dailyRET'.format(output)
with open(filename, 'wb') as f: pickle.dump(data, f)

# HSPF requires potential evapotranspiration (PET) whereas the reference 
# evapotranspiration (RET) calculated with the Penman-Monteith Equation is the 
# PET for well-watered grass. RET may be an appropriate PET value for an HSPF
# model, although the Food and Agriculture Organization (FAO) provides a
# method to extend RET to PET using crop coefficients. The ratio of the PET to 
# the RET is called the crop coefficient and varies throughout the growing 
# season as the plants increase their leaf area index. For example, corn in 
# the Midwestern USA is planted in April and begins with no leaves (or 
# associated transpiration) and is then harvested in the Fall when it has lots 
# of leaves and associated PET. The FAO method divides the crop lifecycle 
# into several phases:
#
#  1. winter to planting
#  2. planting to emergence
#  3. emergence to mid-season (growth phase)
#  4. full growth period
#  5. late growth period to harvest
#  6. harvest to end of year
#
# The daily crop coefficient for a given crop throughout the year can be 
# reduced to the following variables:
#
# plant date
# time to emergence   (step 2 above)
# time to full growth (step 3 above)
# time at full growth (step 4 above)
# late growth time    (step 5 above)
# the crop coefficient for bare soil (no vegetation state)
# the crop coefficient at full growth
# the crop coefficient at harvest
#
# the crop coefficient for a given day can be inferred by constructing a 
# linear function from the bare soil state to full growth state across the 
# growth period, and then from the end of the full growth to the harvest date.
# the FAO has a publication that describes this in detail and includes a 
# number of literature values for the parameters for different crops and
# other kinds of vegetation.
#
# The ETCalculator can be used to calculate daily PET timeseries for crops
# using the FAO method. First, the parameters for a given crop must be provided.
# Corn in the Midwestern USA is planted around April 15 and has approximately
# a 30-day period to emergence, a 50-day growth phase, a 60-day time at full
# growth, and another 40 days until harvest. Literature crop coefficients
# are 0.3 for bare soil, 1.15 for the full growth period, and a harvest 
# coefficient of 0.40 (consistent with FAO guidance for "cereals"). First the
# parameters should be added to the calculator.

# crop growth dates/times (the plant year does not matter)

crop      = 'corn'
plant     = datetime.datetime(2000, 4, 15)
emergence = 30
growth    = 50
full      = 60
late      = 40

# crop coefficients (bare soil, full growth, harvest date)

Ki = 0.30
Km = 1.15
Kl = 0.40

# add the information to the etcalculator

calculator.add_crop(crop, plant, emergence, growth, full, late, Ki, Km, Kl)

# calculate the daily crop coefficient time series from the start to the end 

calculator.calculate_daily_crop(crop, start, end)

# the calculator stores all daily crop coefficient time series in a dictionary
# structure with keys by crop name and values as start datetime and data

start, Kc = calculator.dailyKcs[crop]

# the calculator can extend the reference ET timeseries and the crop coefficient
# to estimate crop-specific PET time series

calculator.daily_PET(crop, start, end)

# the calculator stores the daily PET timeseries in a dictionary structure with
# keys as the crop names and values of the start datetime and data

start, PET = calculator.dailyPETs[crop]

# plot the output

from matplotlib import pyplot, dates, ticker

fig = pyplot.figure(figsize = (8,8))
sub = pyplot.subplot2grid((5,1), (0,0))

# make a time series

times = [start + i * datetime.timedelta(days = 1)
         for i in range((end-start).days)]

# plot the crop coefficient

sub.plot_date(times, Kc, fmt = '-')
sub.set_title('Crop Coefficient')
sub.set_ylabel('PET/RET')
sub.set_ylim((0,1.4))
sub.yaxis.set_major_locator(ticker.MaxNLocator(5))

# plot the RET

sub = pyplot.subplot2grid((5,1), (1,0), rowspan = 2, sharex = sub)

sub.plot_date(times, RET, fmt = '-', color = 'red')
sub.set_title('Reference Evapotranspiration')
sub.set_ylabel('Evapotranspiration\n(mm)')
sub.yaxis.set_major_locator(ticker.MaxNLocator(5))

# plot the PET

sub = pyplot.subplot2grid((5,1), (3,0), sharex = sub, sharey = sub, rowspan = 2)
sub.plot_date(times, PET, color = 'green', fmt = '-')
sub.set_title('Potential Evapotranspiration')
sub.set_ylabel('Evapotranspiration\n(mm)')
sub.xaxis.set_major_locator(dates.MonthLocator((6,12)))
sub.xaxis.set_major_formatter(dates.DateFormatter('%b-%y'))
sub.yaxis.set_major_locator(ticker.MaxNLocator(5))

pyplot.tight_layout()
pyplot.savefig('daily_potential_evapotranspiration')
pyplot.show()

