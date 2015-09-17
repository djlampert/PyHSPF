# perim.py
# 
# David Lampert
#
# ftable02.py
#
# shows how to use the FtableCalculator and the NHDPlus Flowline Value Added
# attributes to extend Ftable estimates from a gage station to other reaches 
# upstream in a watershed. the first part is similar to the previous ftable
# example, but the remainder shows the extension using the NHDPlus data.
#

from shapefile            import Reader
from pyhspf.preprocessing import NWISExtractor, FtableCalculator

import os, pickle, datetime

# NWIS gage station containing gage measurements

gageid = '01594670'

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2010, 1, 1)

# specify the units (English or Metric)

units = 'Metric'

# path to NWIS metadata (will be downloaded if it doesn't exist)

NWIS = 'NWIS'

# path to the flowline shapefile

flowfile = 'data/flowlines.shp'

# create an instance of the NWISExtractor

extractor = NWISExtractor(NWIS)

# download the metadata for all NWIS gages (will be skipped if it exists)

extractor.download_metadata()

# path to the gage metadata

gagefile = '{}/USGS_Streamgages-NHD_Locations.shp'.format(NWIS)

# download all the data for the gage

extractor.download_gagedata(gageid, start, end, output = gageid)

# open up the gage shapefile and use it to get information about the gage

reader = Reader('{}/USGS_Streamgages-NHD_Locations.shp'.format(NWIS))

# find the record index for the NWIS gage ids

i = [f[0] for f in reader.fields].index('SITE_NO') - 1

# find the index of the gage

j = [r[i] for r in reader.records()].index(gageid)

# use the index to get the latitude and longitude of the station

x, y = reader.shape(j).points[0]

# open the flowline shapefile to find the comid of the gage station

reader = Reader(flowfile)

# find shapes with a bounding box encompassing the gage to narrow the search

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

# find the record index of the comids

i = [f[0] for f in reader.fields].index('COMID') - 1

# get the reach common identifier (comid)

comid = record[i]

print('founding comid {} for gageid {}\n'.format(comid, gageid))

# open up the flowline value added attributes

vaafile = 'data/flowlineVAAs'

# the value added attributes is a dictionary with keys corresponding to 
# hydrological sequentce (hydroseq) and values that are instances of PyHSPF's
# Flowline class with a number of attributes about the stream reaches
# including hydrological sequences that describe the reaches that are upstream 
# and downstream of other reaches

with open(vaafile, 'rb') as f: flowlines = pickle.load(f)

# make a dictionary linking comids to their hydrologic sequence id

hydroseqs = {flowlines[f].comid: f for f in flowlines}

# make a list of the comids

comids = [comid]

# make a list of all the comids in the subbasin by moving upstream until 
# reaching the headwaters of the watershed

current = [hydroseqs[comid]]
while len(current) > 0:
    last    = current[:]
    current = []

    for c in hydroseqs:
        if flowlines[hydroseqs[c]].down in last:
            comids.append(c)
            current.append(hydroseqs[c])

print('found {} flowlines including:\n'.format(len(comids)))
for c in comids: print(c)
print('')

# make an instance of the FtableCalculator to use for the data from the file

calculator = FtableCalculator(gageid)

# calculate log-log regressions of flow and width vs depth

calculator.calculate_regressions()

# get the length and average flow from the flowline VAAs

length = flowlines[hydroseqs[comid]].length
qref   = flowlines[hydroseqs[comid]].flow * 0.3048**3

# calculate the FTABLE for the gage station using the length of the reach

ftable = calculator.create_ftable(length, units = units)

print('FTABLE for gage station {}:\n'.format(comid))
for row in ftable: print('{:15.4f} {:15.4f} {:15.4f} {:15.4f}'.format(*row))
print('')

# the FtableCalculator can extend the FTABLES to other reaches upstream
# given their length, average flow, average velocity, and slope and the 
# average flow in the reference reach 

for c in comids:

    # get the flowline VAAs

    flowline = flowlines[hydroseqs[c]]

    length  = flowline.length   # km
    qavg    = flowline.flow     # ft3/s
    
    # convert the units and calculate the slope (set min at 0.0001)

    qavg = qavg * 0.3048**3

    i = c, length, qavg
    print('Reach comid: {}, length: {} km, average flow: {:.4f} cfs'.format(*i))

    ftable = calculator.extend_ftable(qref, qavg, length, units = units)

    print('Estimated FTABLE for reach {}:\n'.format(c))
    for row in ftable: print('{:15.4f} {:15.4f} {:15.4f} {:15.4f}'.format(*row))
    print('')

