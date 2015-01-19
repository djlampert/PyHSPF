# example12.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 01/17/2015
#
# This example illustrates how to break up an 8-digit HUC watershed into 
# subwatersheds that have outlets co-located with dams and flowgages and 
# a maximal drainage area using the PyHSPF HUC8Delineator class. The source
# data files come from NHDPlus V2, NWIS, and the NID as shown in previous
# examples. If you have already downloaded these files, make sure to point
# to the file paths otherwise they will be downloaded automatically (which
# takes a while). The idea is to automate the construction of the data files 
# needed to make the "Watershed" object for PyHSPF.

import os

from pyhspf.preprocessing import HUC8Delineator
from pyhspf.preprocessing import NHDPlusExtractor
from pyhspf.preprocessing import NIDExtractor
from pyhspf.preprocessing import NWISExtractor

# let's work with the Patuxent watershed

HUC8      = '02060006'
state     = 'MD'

# maximum drainage area for individual subbasins (km2)

drainmax = 400

# the land use aggregation file

aggregate = 'data/patuxent/aggregate.csv'

# parallel aggregation flag (if you get errors try turning it off)

parallel = True

# make sure to set the path for the source data directories (the data will
# be downloaded if it doesn't exist)

source      = os.getcwd()
destination = 'HSPF_data'

# make sure the metadata is set before starting the download

print('')
print('preparing to delineate the subbasins for ', HUC8, '\n')
print('if you have already downloaded the NHDPlus, NWIS, and NID source data ')
print('make sure you set the directories, or they will be downloaded again')
print('press "y" to continue...\n')

s = 'n'
while s != 'y':
    s = input()
    if s == 'n':  exit()
    else: continue

# source data directory structure (ideally a data drive or server)

NHDPlus = '{}/NHDPlus'.format(source)
NWIS    = '{}/NWIS'.format(source)
NID     = '{}/NID'.format(source)

# download and extract the data using the PyHSPF data extractors (if needed)
# these steps can/will be skipped if they are not needed

nhdplusextractor = NHDPlusExtractor(HUC8[:2], NHDPlus)
nwisextractor    = NWISExtractor(NWIS)
nidextractor     = NIDExtractor(NID)

# extract the NHDPlus data

nhdplusextractor.download_data()
nhdplusextractor.extract_HUC8(HUC8, destination)

# this is the destination directory created for the NHDPlus data for the HUC8

output = '{}/{}'.format(destination, HUC8)

# paths to the NHDPlus data files created above by the nhdplusextractor

bfile = '{}/{}/boundary'.format(destination, HUC8)     # watershed boundary
cfile = '{}/{}/catchments'.format(destination, HUC8)   # individual catchments
ffile = '{}/{}/flowlines'.format(destination, HUC8)    # individual flowlines
VAAs  = '{}/{}/flowlineVAAs'.format(destination, HUC8) # value-added attributes
efile = '{}/{}/elevations'.format(destination, HUC8)   # elevation geotiff

# extract the NWIS gages to a shapefile in the new HUC8 directory

nwisextractor.extract_HUC8(HUC8, output)

# path to the gage file created above

gfile = '{}/{}/gagestations'.format(destination, HUC8)

# extract the NID dams to a shapefile in the new HUC8 directory

dfile = '{}/{}/dams'.format(destination, HUC8)

nidextractor.extract_shapefile(bfile, dfile)

# use the locations of the gages and dams, the NHDPlus data files, and
# PyHSPF's HUC8Delineator to delineate subbasins subject to the criteria
# into a new file in the HUC8 output directory

delineator = HUC8Delineator(HUC8, VAAs, ffile, cfile, efile, gfile, dfile)

# delineate the watershed using the NHDPlus data and delineator

delineator.delineate(destination, drainmax = drainmax, parallel = parallel)

# the delineation can be sped up by changing the parallel flag to True, and it
# a list of extra outlets can be supplied using the "extra_outlets" keyword
# argument if there are other points of interest in the HUC8 where subbasins 
# should be split. this script should have generated a bunch of files in the
# output directory including a few PNG images of the prelimary and delineated
# flowlines and catchments.
