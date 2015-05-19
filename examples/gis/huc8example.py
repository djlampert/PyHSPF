# huc8example.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 01/20/2015
#
# This example illustrates how to break up an 8-digit HUC watershed into 
# subwatersheds that have outlets co-located with dams and flowgages and 
# a maximal drainage area using the PyHSPF HUC8Delineator class. The source
# data files come from NHDPlus V2, NWIS, and the NID as shown in previous
# examples. The catchments and flowlines from NHDPlus are aggregated together
# to satisfy the criteria for the delineation. If you have already downloaded 
# the source data files from NHDPlus, NWIS, and NID, make sure to point
# to the file paths otherwise they will be downloaded automatically (which
# takes a while). The idea is to automate the construction of the data files 
# needed to make the "Watershed" object for PyHSPF.

import os

from pyhspf.preprocessing import HUC8Delineator
from pyhspf.preprocessing import NHDPlusExtractor
from pyhspf.preprocessing import NIDExtractor
from pyhspf.preprocessing import NWISExtractor

# 8-digit hydrologic code for the Patuxent watershed

HUC8 = '02060006'

# maximum drainage area for individual subbasins (in km2)

drainmax = 400

# parallel aggregation flag (if you get errors try turning it off)

parallel = True

# make sure to set the path for the source data directories (the data will
# be downloaded if it doesn't exist)

source      = os.getcwd()
destination = 'HSPF_data'

# make a destination directory for all HSPF data

if not os.path.isdir(destination): os.mkdir(destination)

# make a destination subdirectory for the HSPF data for the HUC8

output = '{}/{}'.format(destination, HUC8)

# to use multiprocessing on Windows it is necessary to create a routine and
# call it below

def main():

    # make sure the metadata are set before starting the download

    print('')
    print('preparing to delineate the subbasins for HUC {}\n'.format(HUC8))
    print('if you have already downloaded the NHDPlus, NWIS, and NID source ' +
          'data make sure you set the directory paths, or all the data will ' +
          'be downloaded again.\n')
    print('press "y" to continue or "n" to abort...\n')

    s = 'n'
    while s != 'y':
        s = input()
        if s == 'n':  exit()

    # source data directory structure (ideally a data drive or server)

    NHDPlus = '{}/NHDPlus'.format(source)
    NWIS    = '{}/NWIS'.format(source)
    NID     = '{}/NID'.format(source)

    # download and extract the data using the PyHSPF data extractors (if needed)
    # these steps can/will be skipped if they are not needed

    nhdplusextractor = NHDPlusExtractor(HUC8[:2], NHDPlus)
    nwisextractor    = NWISExtractor(NWIS)
    nidextractor     = NIDExtractor(NID)

    # extract or set the path to the source NHDPlus data

    nhdplusextractor.download_data()

    # extract the hydrography data for the HUC8 to the output directory

    nhdplusextractor.extract_HUC8(HUC8, output)

    # paths to the NHDPlus data files created above by the nhdplusextractor

    bfile = '{}/boundary'.format(output)     # watershed boundary
    cfile = '{}/catchments'.format(output)   # individual catchments
    ffile = '{}/flowlines'.format(output)    # individual flowlines
    VAAs  = '{}/flowlineVAAs'.format(output) # value-added attributes
    efile = '{}/elevations'.format(output)   # elevation geotiff

    # extract the NWIS gages to a shapefile in the HUC8 directory

    nwisextractor.extract_HUC8(HUC8, output)

    # path to the gage shapefile created above

    gfile = '{}/gagestations'.format(output, HUC8)

    # extract the NID dams to a shapefile in the new HUC8 directory

    dfile = '{}/dams'.format(output, HUC8)

    nidextractor.extract_shapefile(bfile, dfile)

    # use the locations of the gages and dams, the NHDPlus data files, and
    # PyHSPF's HUC8Delineator to delineate subbasins subject to the criteria
    # into a new file in the HUC8 output directory

    delineator = HUC8Delineator(HUC8, VAAs, ffile, cfile, efile, gfile, dfile)

    # delineate the watershed using the NHDPlus data and delineator

    delineator.delineate(output, drainmax = drainmax, parallel = parallel)

# the delineation process will take advantage of multiprocessing if the
# parallel flag to "True," although this consumes memory and is less stable.
# a list of extra outlets can be supplied using the "extra_outlets" keyword
# argument to the delineate method if there are other points of interest 
# in the HUC8 where you want to place subbasin outlets (besides dams and gages).
# this script generates a bunch of files in the "output" directory including a 
# few PNG images of the prelimary and delineated flowlines and catchments.

if __name__ == '__main__': main()
