# ftable01.py
#
# author: David Lampert (djlampert@gmail.com)
#
# last updated: 09/03/2015
#
# illustrates how to download gage data from NWIS, find information about a
# reach using an NHDPlus flowline shapefile, and finally use the
# FtableCalculator to create FTABLES for gage stations for HSPF.
#

import os, datetime
import pickle
import json
from shapefile            import Reader
from pyhspf.preprocessing import NWISExtractor, FtableCalculator

# path to NWIS metadata (will be downloaded if it doesn't exist)

NWIS = 'NWIS'



# start and end dates (not really relevant for this example, but needed to use
# the download tools)
cwd = os.path.dirname(__file__)
start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2010, 1, 1)

# specify the units (English or Metric)

# name of file containing the NWIS data (will be downloaded if it doesn't exist)

gageid = '01594670'
gageidpath = os.path.join(cwd,gageid)
units = 'Metric'

# shapefile containing NHDPlus flowlines and associated attributes

flowfile = os.path.join(cwd,'data/flowlines.shp')

# name of the plot generated showing the results

plotname = 'ftable'
plotnamepath = os.path.join(cwd, plotname)

# make sure the flowline file exists

if not os.path.isfile(flowfile):
    print('Please ensure that the file {} exists\n'.format(flowfile))
    raise

# create an instance of the NWISExtractor to use to download the gage data

extractor = NWISExtractor(NWIS)

# download the metadata for all NWIS gages (will be skipped if it exists)

extractor.download_metadata()

# download all the data for the gage, including measured values of stage,
# discharge, channel width, and channel area and save it to "gageid" file
# (will be skipped if it already exists)

extractor.download_gagedata(gageid, start, end, output = gageidpath)


# need to know the reach length; so find the location of the gage, then find
# the flowline in the shapefile and use the record info to get the length

# first use the NWIS metadata file to get the latitude and longitude of the gage

reader = Reader('{}/USGS_Streamgages-NHD_Locations.shp'.format(NWIS))

# find the record index for the NWIS gage ids

i = [f[0] for f in reader.fields].index('SITE_NO') - 1

# find the index of the gage

j = [r[i] for r in reader.records()].index(gageid)

# use the index to get the latitude and longitude of the station

x, y = reader.shape(j).points[0]

print('location of gage {}: {:.4f}, {:.4f}\n'.format(gageid, x, y))

# open the flowline shapefile to supply reach length (miles or kilometers
# depending on the unit system)

reader = Reader(flowfile)

# find shapes with a bounding box encompassing the gage to narrow the search

print('searching for the closest flowline to the gage\n')

bboxes = [s.bbox for s in reader.shapes()]

contains = [i for i, b in enumerate(bboxes)
            if b[0] <= x and x <= b[2] and b[0] <= y and y <= b[3]]

# find the distances between all the overlapping shapes points and the gage

distances = [min([(x1 - x)**2 + (y1 - y)**2
                  for x1, y1 in reader.shape(i).points])
             for i in contains]

# find the shape with the point closest to the gage

closest = contains[distances.index(min(distances))]

# read the record for the flowline

record = reader.record(closest)

# find the record indices of the comid and reach length in km in the file

i = [f[0] for f in reader.fields].index('LENGTHKM') - 1
j = [f[0] for f in reader.fields].index('COMID') - 1

# get the reach length and common identifier

length = record[i]
comid  = record[j]

it = comid, length
print('comid {} is closest to the gage and has a length of {} km\n'.format(*it))

# make an instance of the FtableCalculator to use for the data from the file

calculator = FtableCalculator(gageidpath)

# calculate log-log regressions of flow and width vs depth using the observed
# values from the NWIS database

calculator.calculate_regressions()

# calculate the FTABLE using the length of the reach and the regressions for:
#
# the width (surface area = width * length),
# the volume (volume = width * depth * length)
# the discharge
#
# this approach utilizes all available data to infer discharge rate for a given
# depth of flow needed for continuous simulation

ftable = calculator.create_ftable(length, units = units)

# the FTABLE is a list of up lists with up to 18 values of depths and the
# corresponding surface areas, volumes, and discharges that HSPF uses to
# interpolate for a reach at each time step.

print('FTABLE for the reach:\n')

if units == 'Metric':

    row = ['Depth (m)', 'Area (ha)', 'Volume (Mm3)', 'Flow (m3/s)']

elif units == 'English':

    row = ['Depth (ft)', 'Area (acre)', 'Volume (acre-ft)', 'Flow (ft3/s)']

else:

    print('error: unknown units "{}" specified'.format(units))

print('{:>15s} {:>15s} {:>15s} {:>15s}'.format(*row))

for row in ftable:

    print('[{:14.4f}, {:14.5f}, {:14.6f}, {:14.5f}],'.format(*row))

# make a plot of the regression and the individual values for the table

print('\nplotting the results\n')

calculator.plot_regressions(output = plotnamepath)

# the steps above can be automated and customized as needed depending on the
# available information (in this case the reach info was supplied by the data
# in the NHDPlus flowline file that can be generated as shown elsewhere)
