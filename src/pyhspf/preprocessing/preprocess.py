#!/usr/bin/env python3
#
###############################################################################
#                                                                             #
# Python Upper Mississippi River Basin HSPF Preprocessor                      #
#                                                                             #
# David J. Lampert, PhD, PE                                                   #
#                                                                             #
# last updated: 09/26/2014                                                    #
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
# The Prinicipal Aquifers of the United States shapefile                      #
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/aquifers_us.xml              #
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
from .delineators         import UMRBDelineator#, NHDPlusDelineator
from .nwisextractor       import NWISExtractor
from .nidextractor        import NIDExtractor
from .extract_gis_data    import extract_gis_data
from .subdivide_watershed import subdivide_watershed
from .make_subbasins      import make_subbasins
from .calculate_landuse   import calculate_landuse
from .landuse_stats       import landuse_stats
from .build_watershed     import build_watershed
from .make_gagestations   import make_gagestations
from .download_climate    import download_climate
from .extract_climate     import extract_climate

def preprocess(network, 
               output, 
               HUC8, 
               state, 
               start, 
               end,
               landcodes = 'aggregate.csv', 
               drainmin = 0, 
               drainmax = 400, 
               extra_outlets = None,
               overwrite = False, 
               verbose = True, 
               vverbose = False, 
               parallel = True, 
               extract = True, 
               subdivide = True, 
               subbasins = True, 
               landuse = True, 
               landstats = True, 
               build = True, 
               climate = True, 
               gagedata = True,
               subbasinplots = False, 
               watershedplots = True, 
               landplots = True,
               landpercents = False, 
               flowplots = True, 
               metstatplots = True,
               metgageplots = True,
               ):
    """Preprocess the data for HSPF."""

    go = time.time()

    # source data locations on the network

    #NHDPlus  = '{}/NHDPlus/NHDPlusMS/NHDPlus07'.format(network)
    NHDPlus  = '{}/NHDPlus'.format(network)
    NED      = '{}/NEDSnapshot'.format(NHDPlus)
    NWIS     = '{}/NWIS'.format(network)
    NASS     = '{}/NASS'.format(network)
    NID      = '{}/NID'.format(network)
    AQUI     = '{}/Aquifers/aquifrp025'.format(network)

    # file paths

    gagepath     = '{0}/{1}/NWIS'.format(output, HUC8)
    subbasinfile = '{0}/{1}/subbasins'.format(output, HUC8)
    inletfile    = '{0}/{1}/subbasin_inlets'.format(output, HUC8)
    outletfile   = '{0}/{1}/subbasin_outlets'.format(output, HUC8)
    gagefile     = '{0}/{1}/gagestations'.format(output, HUC8)
    damfile      = '{0}/{1}/dams'.format(output, HUC8)
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

        #extract_gis_data(NHDPlus, NED, NWIS, NASS, NID, AQUI, RPU, HUC8, state,
        #                 years, outputpath = output, parallel = parallel, 
        #                 overwrite = overwrite, verbose = verbose,
        #                 vverbose = vverbose)

    # create an instance of the UMRBDelineator to use to delineate the watershed

    delineator = UMRBDelineator(HUC8, VAAfile, flowfile, cfile, elevfile,
                                gagefile, damfile)

    # delineate the watershed using the NHDPlus data and delineator

    delineator.delineate(output, 
                         drainmax = drainmax, 
                         extra_outlets = extra_outlets,
                         verbose = vverbose,
                         )

    # subdivide the watershed to meet the subbasin criteria

    #subbasins = delineator.make_subbasin_outlets(HUC8,
    #                                             outletfile,
    #                                             inletfile,
    #                                             subbasinfile,
    #                                             drainmax = drainmax, 
    #                                             extras = extra_outlets, 
    #                                             verbose = verbose
    #                                             )
    

#    if subdivide:
    if 1==2:
        subdivide_watershed(HUC8, flowfile, gagefile, damfile, 
                            drainmin = drainmin, 
                            drainmax = drainmax, extra_outlets = extra_outlets,
                            outputpath = output, verbose = verbose, 
                            vverbose = vverbose) 

    # make the subbasins from the outlets (time-consuming)

    if 1==2:
    #if subbasins:
        make_subbasins(HUC8, output, subbasin_plots = subbasinplots, 
                       watershed_plots = watershedplots, parallel = 
                       parallel, verbose = verbose, vverbose = vverbose)

    # calculate the landuse statistics for each year and each subbasin

    if 1==2:
    #if landuse:
        calculate_landuse(output, HUC8, years, landcodes, overwrite = 
                          overwrite, parallel = parallel, verbose = verbose)

    # tabulate the land use statistics

    if landpercents:
        landusepercent = output + '/%s/%slandpercent.csv' % (HUC8, HUC8)
        landuse_stats(output, HUC8, landcodes, subbasinfile, landusepercent,
                      units = 'percent', overwrite = overwrite,
                      verbose = verbose, vverbose = vverbose)

    if 1==2:
    #if landstats:
        landusekm = output + '/%s/%slanduse.csv'     % (HUC8, HUC8)
        landuse_stats(output, HUC8, landcodes, subbasinfile, landusekm, 
                      picklefile = landfile, units = 'sqkm', 
                      overwrite = overwrite, plots = landplots, 
                      verbose = verbose, vverbose = vverbose) 

    # build the watershed object

    if 1==2:
    #if build:
        flowlinefile = output + '/%s/%ssubbasin_flowlines' % (HUC8, HUC8)
        VAAfile  = output + '/%s/flowlineVAAs'    % HUC8
        build_watershed(subbasinfile, flowlinefile, outletfile, damfile, 
                        gagefile, landfile, landcodes, VAAfile, 
                        years, HUC8, output, plots = flowplots)

    # extract gage station data

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end, 1, 1)

    if 1==2:
    #if gagedata:
        make_gagestations(output, HUC8, state, s, e, verbose = verbose)

    # extract climate data

    if 1==2:
    #if climate:
        download_climate(output, HUC8, s, e)
        extract_climate(output, HUC8, s, e)

    # make a directory for HSPF calculations

    hspfdirectory = '{}/{}/hspf'.format(output, HUC8)
    if not os.path.isdir(hspfdirectory): os.mkdir(hspfdirectory)

    if verbose: 
        print('\ncompleted preprocessing watershed in %.1f seconds\n' % 
              (time.time() - go))

