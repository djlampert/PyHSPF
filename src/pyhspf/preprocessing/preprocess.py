#!/usr/bin/env python3
#
###############################################################################
#                                                                             #
# Python Upper Mississippi River Basin HSPF Preprocessor                      #
#                                                                             #
# David J. Lampert, PhD, PE                                                   #
#                                                                             #
# last updated: 10/09/2014                                                    #
#                                                                             #
# Purpose: Extracts GIS data from sources and builds the input file for HSPF  #
# for a given set of assumptions for a USGS 8-digit Watershed. Details below. #
#                                                                             #
###############################################################################
#
###############################################################################
#                                                                             #
# Paths to source data directories are all available online. Note that these  #
# can be large so you may have organize as I did below.                       #
#                                                                             #
# National Hydrography Dataset Plus (NHDPlus); should use directory structure #
# specified in the source at http://www.horizon-systems.com/nhdplus/          #
#                                                                             #
# The National Elevation Dataset (NED) on a 30-m grid (supplied with NHDPlus) #
#                                                                             #
# The land use geotiff file(s); for this case the National Agricultural       #
# Statistics Service (NASS).  Can be function of time.                        #
#                                                                             #
# The National Inventory of Dams (NID) shapefile available at                 #
# http://nationalatlas.gov/metadata/dams00x020.faq.html                       #
#                                                                             #
# The USGS NWIS gage station shapefile available at                           #
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgages.xml              #
#                                                                             #
# The time series data for the climate are downloaded automatically from the  #
# National Climate Data Center (NCDC) and the National Solar Radiation        #
# Database (NSRDB)                                                            #
#                                                                             #
###############################################################################

import os, time, datetime

# local imports

from .nhdplusextractor    import NHDPlusExtractor
from .nwisextractor       import NWISExtractor
from .nidextractor        import NIDExtractor
from .delineators         import HUC8Delineator
from .cdlextractor        import CDLExtractor
from .build_watershed     import build_watershed
from .climateprocessor    import ClimateProcessor
#from .download_climate    import download_climate
#from .extract_climate     import extract_climate

def preprocess(network, 
               output, 
               HUC8, 
               state, 
               start, 
               end,
               landcodes = 'aggregate.csv', 
               drainmin = 0, 
               drainmax = 400, 
               extra_outlets  = None,
               overwrite      = False, 
               verbose        = True, 
               vverbose       = False, 
               parallel       = True, 
               extract        = True, 
               delineate      = True,
               landuse        = True, 
               landstats      = True, 
               build          = True, 
               climate        = True, 
               gagedata       = True,
               subbasinplots  = False, 
               watershedplots = True, 
               landplots      = True,
               landpercents   = False, 
               flowplots      = True, 
               metstatplots   = True,
               metgageplots   = True,
               ):
    """Preprocess the data for HSPF."""

    go = time.time()

    # source data locations on the network

    NHDPlus  = '{}/NHDPlus'.format(network)
    NED      = '{}/NEDSnapshot'.format(NHDPlus)
    NWIS     = '{}/NWIS'.format(network)
    CDL      = '{}/CDL'.format(network)
    NID      = '{}/NID'.format(network)

    # file paths

    gagepath     = '{0}/{1}/NWIS'.format(output, HUC8)
    subbasinfile = '{0}/{1}/subbasin_catchments'.format(output, HUC8)
    inletfile    = '{0}/{1}/subbasin_inlets'.format(output, HUC8)
    outletfile   = '{0}/{1}/subbasin_outlets'.format(output, HUC8)
    gagefile     = '{0}/{1}/gagestations'.format(output, HUC8)
    damfile      = '{0}/{1}/dams'.format(output, HUC8)
    landusedata  = '{0}/{1}/landuse'.format(output, HUC8)
    landfile     = '{0}/{1}/subbasinlanduse'.format(output, HUC8)
    VAAfile      = '{0}/{1}/flowlineVAAs'.format(output, HUC8)
    bfile        = '{0}/{1}/boundary'.format(output, HUC8)
    cfile        = '{0}/{1}/catchments'.format(output, HUC8)
    flowfile     = '{0}/{1}/flowlines'.format(output, HUC8)
    elevfile     = '{0}/{1}/elevations'.format(output, HUC8)

    years = [year for year in range(start, end)]

    # extract the data for the HUC8 from the sources

    if extract:

        # create an instance of the extractor

        nhdplusextractor = NHDPlusExtractor(HUC8[:2], NHDPlus)

        # extract the HUC8 data for the Patuxent watershed

        nhdplusextractor.extract_HUC8(HUC8, output)

        # extract the gage data from NWIS

        nwisextractor = NWISExtractor(NWIS)

        # extract the gage stations into a new shapefile for the HUC8 and place
        # them into the newly generated directory for the HUC8

        nwisextractor.extract_HUC8(HUC8, '{}/{}'.format(output, HUC8))

        # download all the gage data to the gagepath directory

        nwisextractor.download_all(datetime.datetime(start, 1, 1),
                                   datetime.datetime(end, 1, 1), 
                                   output = gagepath)

        # extract the dam data from the NID

        nidextractor = NIDExtractor(NID)

        # extract dams in the HUC8 using the shapefile for the boundary

        nidextractor.extract_shapefile(bfile, damfile)

    if delineate:

        # create an instance of the UMRBDelineator to delineate the watershed

        delineator = HUC8Delineator(HUC8, 
                                    VAAfile, 
                                    flowfile, 
                                    cfile, 
                                    elevfile,
                                    gagefile, 
                                    damfile
                                    )

        # delineate the watershed using the NHDPlus data and delineator

        delineator.delineate(output, 
                             parallel      = parallel,
                             drainmax      = drainmax, 
                             extra_outlets = extra_outlets,
                             verbose       = verbose,
                             vverbose      = vverbose,
                             )

    if landuse:

        # extract cropland data from NASS

        cdlextractor = CDLExtractor(CDL)

        # download the data for each state for each year

        cdlextractor.download_data(state.upper(), years)

        # make a directory for the CDL files

        if not os.path.isdir(landusedata): os.mkdir(landusedata)

        # extract the data for the watershed using the boundary shapefile

        if not any([os.path.isfile('{}/{}landuse.tif'.format(landusedata, year))
                    for year in years]):
                                   
            cdlextractor.extract_shapefile(bfile, landusedata)

        # calculate the landuse in each subbasin for each year

        for year in years:

            attribute = 'ComID'
            extracted = '{}/{}landuse.tif'.format(landusedata, year)

            # csv file of the output

            csvfile = '{}/{}landuse.csv'.format(landusedata, year)
            if not os.path.isfile(csvfile):
                cdlextractor.calculate_landuse(extracted, subbasinfile, 
                                               landcodes, attribute,
                                               csvfile = csvfile)

            # raw landuse plot

            raw = '{}/{}raw'.format(landusedata, year)
            if not os.path.isfile(raw + '.png'):
                cdlextractor.plot_landuse(extracted, subbasinfile, attribute, 
                                          output = raw, lw = 2.,
                                          datatype = 'raw')

            # aggregated land use plot

            results = '{}/{}results'.format(landusedata, year)
            if not os.path.isfile(results + '.png'):
                cdlextractor.plot_landuse(extracted, subbasinfile, attribute, 
                                          output = results, 
                                          datatype = 'results')

        print('')

    # build the watershed object

    if build:

        if not os.path.isfile('{}/{}/watershed'.format(output, HUC8)):
            flowlinefile = '{}/{}/subbasin_flowlines'.format(output, HUC8)
            VAAfile      = '{}/{}/flowlineVAAs'.format(output, HUC8)
            build_watershed(subbasinfile, flowlinefile, outletfile, damfile, 
                            gagefile, landusedata, VAAfile, 
                            years, HUC8, output, plots = flowplots)

    # extract gage station data

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end, 1, 1)

    # download and extract the climate data

    if climate:

        climatedata = '{}/{}/climate'.format(output, HUC8)

        #extract_climate(output, HUC8, s, e)

        if not os.path.isdir(climatedata): os.mkdir(climatedata)
        climateprocessor = ClimateProcessor()
        climateprocessor.download_shapefile(subbasinfile, s, e, climatedata,
                                            space = 0.5)



    # make a directory for HSPF calculations

    hspfdirectory = '{}/{}/hspf'.format(output, HUC8)
    if not os.path.isdir(hspfdirectory): os.mkdir(hspfdirectory)

    if verbose: 
        print('completed preprocessing watershed in %.1f seconds\n' % 
              (time.time() - go))

